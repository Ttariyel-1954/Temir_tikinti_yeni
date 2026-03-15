# ╔══════════════════════════════════════════════════════════════════════╗
# ║  TƏHSİL NAZİRLİYİ — TƏMİR TİKİNTİ İDARƏSİ                        ║
# ║  Süni İntellektlə İdarəetmə Sistemi  ·  Shiny R                    ║
# ╚══════════════════════════════════════════════════════════════════════╝

library(shiny)
library(bslib)
library(DBI)
library(RPostgres)
library(pool)
library(DT)
library(dplyr)
library(plotly)
library(httr2)
library(jsonlite)
library(shinyjs)
library(shinycssloaders)
library(openxlsx)

# ── .env ──────────────────────────────────────────────────────────────
env_file <- file.path(getwd(), ".env")
if (file.exists(env_file)) {
  env_lines <- readLines(env_file, warn = FALSE)
  for (line in env_lines) {
    line <- trimws(line)
    if (nchar(line) == 0 || startsWith(line, "#")) next
    parts <- strsplit(line, "=", fixed = TRUE)[[1]]
    if (length(parts) >= 2) {
      key <- trimws(parts[1])
      val <- trimws(paste(parts[-1], collapse = "="))
      do.call(Sys.setenv, setNames(list(val), key))
    }
  }
  cat(".env yuklendi\n")
}

# ══════════════════════════════════════════════════════════════════════
#  1. DATABASE
# ══════════════════════════════════════════════════════════════════════

db_args <- list(
  drv    = RPostgres::Postgres(),
  dbname = Sys.getenv("DB_NAME", "temir_tikinti"),
  host   = Sys.getenv("DB_HOST", "localhost"),
  port   = as.integer(Sys.getenv("DB_PORT", "5432"))
)
if (nzchar(Sys.getenv("DB_USER")))     db_args$user     <- Sys.getenv("DB_USER")
if (nzchar(Sys.getenv("DB_PASSWORD"))) db_args$password <- Sys.getenv("DB_PASSWORD")
pool <- do.call(dbPool, db_args)
onStop(function() poolClose(pool))

# ── Sorğular ──────────────────────────────────────────────────────────

get_stats <- function() {
  qn <- function(sql) as.numeric(dbGetQuery(pool, sql)$n)
  list(
    mekteb   = qn("SELECT count(*)::int as n FROM mektebler WHERE aktivdir"),
    bina     = qn("SELECT count(*)::int as n FROM binalar WHERE aktivdir"),
    isci     = qn("SELECT count(*)::int as n FROM isciler WHERE aktivdir"),
    aktiv    = qn("SELECT count(*)::int as n FROM layiheler WHERE veziyyet NOT IN ('tamamlanıb','ləğv edilib')"),
    tamamlan = qn("SELECT count(*)::int as n FROM layiheler WHERE veziyyet = 'tamamlanıb'"),
    budce    = qn("SELECT COALESCE(sum(plan_budce),0) as n FROM layiheler"),
    xerc     = qn("SELECT COALESCE(sum(real_xerc),0) as n FROM layiheler"),
    kritik   = qn("SELECT count(*)::int as n FROM layiheler WHERE prioritet='kritik' AND veziyyet!='tamamlanıb'"),
    problem  = qn("SELECT count(*)::int as n FROM binalar WHERE veziyyet IN ('pis','qəza') AND aktivdir"),
    material = qn("SELECT count(*)::int as n FROM materiallar WHERE aktivdir"),
    az_mat   = qn("SELECT count(*)::int as n FROM materiallar WHERE anbar_sayi <= minimum_say AND aktivdir")
  )
}

get_layiheler <- function(vez = NULL, pri = NULL) {
  sql <- "SELECT l.id, l.ad AS layihe, l.veziyyet, l.prioritet,
          l.plan_baslama, l.plan_bitis, l.plan_budce, l.real_xerc,
          l.tamamlanma_faizi AS faiz, b.ad AS bina, m.ad AS mekteb,
          tk.ad AS kateqoriya, COALESCE(i.ad||' '||i.soyad,'-') AS masul
   FROM layiheler l
   JOIN binalar b ON l.bina_id=b.id JOIN mektebler m ON b.mekteb_id=m.id
   JOIN temir_kateqoriyalar tk ON l.kateqoriya_id=tk.id
   LEFT JOIN isciler i ON l.masul_isci_id=i.id WHERE 1=1"
  params <- list()
  if (!is.null(vez) && vez != "Hamısı") { sql <- paste0(sql, " AND l.veziyyet=$1"); params <- c(params, vez) }
  if (!is.null(pri) && pri != "Hamısı") { sql <- paste0(sql, " AND l.prioritet=$", length(params)+1); params <- c(params, pri) }
  sql <- paste0(sql, " ORDER BY l.plan_baslama DESC")
  if (length(params) > 0) dbGetQuery(pool, sql, params = params) else dbGetQuery(pool, sql)
}

get_mektebler <- function(bolge = NULL) {
  sql <- "SELECT m.id, m.ad, m.kod, m.tip, m.unvan, m.sagird_sayi, m.muellim_sayi, m.sinif_sayi,
          COALESCE(r.ad,'-') AS rayon, bo.ad AS bolge, m.qarabag_qayidis
   FROM mektebler m LEFT JOIN rayon_sehirler r ON m.rayon_id=r.id
   LEFT JOIN bolgeler bo ON m.bolge_id=bo.id WHERE m.aktivdir"
  if (!is.null(bolge) && bolge != "Hamısı") {
    dbGetQuery(pool, paste0(sql, " AND bo.ad=$1 ORDER BY m.ad"), params = list(bolge))
  } else { dbGetQuery(pool, paste0(sql, " ORDER BY bo.ad, m.ad")) }
}

get_binalar <- function(vez = NULL) {
  sql <- "SELECT b.id, b.ad AS bina, b.bina_novu, b.tikilis_ili, b.mertebe_sayi,
          b.umumi_sahe, b.sinif_otaq_sayi, b.veziyyet, b.dam_tipi, b.istilik_sistemi,
          b.son_temir_ili, b.yangin_siqnal, b.generator_var, m.ad AS mekteb
   FROM binalar b JOIN mektebler m ON b.mekteb_id=m.id WHERE b.aktivdir"
  if (!is.null(vez) && vez != "Hamısı") {
    dbGetQuery(pool, paste0(sql, " AND b.veziyyet=$1 ORDER BY m.ad"), params = list(vez))
  } else { dbGetQuery(pool, paste0(sql, " ORDER BY b.veziyyet DESC, m.ad")) }
}

get_isciler <- function() {
  dbGetQuery(pool, "SELECT i.id, i.ad, i.soyad, i.ata_adi, i.ise_baslama, i.maas,
   i.telefon, i.email, v.ad AS vezife, v.kateqoriya, COALESCE(b.ad,'-') AS bolge
   FROM isciler i JOIN vezifeler v ON i.vezife_id=v.id
   LEFT JOIN bolgeler b ON i.bolge_id=b.id WHERE i.aktivdir ORDER BY v.kateqoriya, i.soyad")
}

get_materiallar <- function() {
  dbGetQuery(pool, "SELECT id, ad, kateqoriya, olcu_vahidi, vahid_qiymeti, anbar_sayi, minimum_say,
   CASE WHEN anbar_sayi<=minimum_say THEN 'Bəli' ELSE 'Xeyr' END AS az_qalib
   FROM materiallar WHERE aktivdir ORDER BY kateqoriya, ad")
}

get_podratcilar <- function() {
  dbGetQuery(pool, "SELECT id,ad,voen,telefon,email,ixtisas,reytinq FROM podratcilar WHERE aktivdir ORDER BY reytinq DESC")
}

get_inspeksiyalar <- function() {
  dbGetQuery(pool, "SELECT ins.id, ins.tarix, ins.netice, ins.umumi_bal, ins.qeydler,
   ins.narahatliqlar, ins.tovsiyeler, ins.novbeti_tarix, b.ad AS bina, m.ad AS mekteb,
   COALESCE(i.ad||' '||i.soyad,'-') AS inspektor
   FROM inspeksiyalar ins JOIN binalar b ON ins.bina_id=b.id
   JOIN mektebler m ON b.mekteb_id=m.id LEFT JOIN isciler i ON ins.inspektor_id=i.id ORDER BY ins.tarix DESC")
}

get_bolge_stat <- function() {
  dbGetQuery(pool, "SELECT bo.ad AS bolge, bo.emsal,
   count(DISTINCT m.id)::int AS mekteb_sayi, count(DISTINCT b.id)::int AS bina_sayi,
   count(DISTINCT l.id)::int AS layihe_sayi, COALESCE(SUM(l.plan_budce),0) AS umumi_budce,
   count(DISTINCT CASE WHEN b.veziyyet IN ('pis','qəza') THEN b.id END)::int AS problem_bina
   FROM bolgeler bo LEFT JOIN mektebler m ON m.bolge_id=bo.id AND m.aktivdir
   LEFT JOIN binalar b ON b.mekteb_id=m.id AND b.aktivdir
   LEFT JOIN layiheler l ON l.bina_id=b.id WHERE bo.aktivdir
   GROUP BY bo.id, bo.ad, bo.emsal ORDER BY bo.ad")
}

get_tehcizat <- function() {
  dbGetQuery(pool, "SELECT t.id,t.ad,t.kateqoriya,t.miqdar,t.vahid_qiymeti,
   t.\"alış_tarixi\",t.zemanet_bitis,t.veziyyet, m.ad AS mekteb
   FROM tehcizat t JOIN mektebler m ON t.mekteb_id=m.id ORDER BY t.kateqoriya,t.ad")
}

get_senedler <- function() {
  dbGetQuery(pool, "SELECT s.id,s.sened_novu,s.ad,s.sened_no,s.tarix,
   COALESCE(l.ad,'-') AS layihe, COALESCE(m.ad,'-') AS mekteb
   FROM senedler s LEFT JOIN layiheler l ON s.layihe_id=l.id
   LEFT JOIN mektebler m ON s.mekteb_id=m.id ORDER BY s.tarix DESC")
}

get_bolmeler <- function() {
  dbGetQuery(pool, "SELECT b.id,b.ad,b.kod,b.seviyye, COALESCE(p.ad,'-') AS ust_bolme,
   COALESCE(i.ad||' '||i.soyad,'-') AS sef FROM bolmeler b
   LEFT JOIN bolmeler p ON b.ust_id=p.id LEFT JOIN isciler i ON b.sef_id=i.id
   WHERE b.aktivdir ORDER BY b.seviyye, b.ad")
}

get_xeberdarlilar <- function() {
  alerts <- list()
  geciken <- dbGetQuery(pool, "SELECT ad, plan_bitis, tamamlanma_faizi FROM layiheler
   WHERE plan_bitis < CURRENT_DATE AND veziyyet NOT IN ('tamamlanıb','ləğv edilib')")
  for (i in seq_len(nrow(geciken)))
    alerts[[length(alerts)+1]] <- list(tip="gecikme", seviye="kritik",
      mesaj=sprintf("'%s' — %s tarixində bitməli idi, hələ %d%% tamamlanıb",
                    geciken$ad[i], geciken$plan_bitis[i], geciken$tamamlanma_faizi[i]))
  asim <- dbGetQuery(pool, "SELECT ad,plan_budce,real_xerc FROM layiheler WHERE real_xerc>plan_budce AND plan_budce>0")
  for (i in seq_len(nrow(asim)))
    alerts[[length(alerts)+1]] <- list(tip="budce", seviye="yuksek",
      mesaj=sprintf("'%s' — büdcə aşımı: plan %.0f, real %.0f AZN", asim$ad[i], asim$plan_budce[i], asim$real_xerc[i]))
  qeza <- dbGetQuery(pool, "SELECT b.ad AS bina, m.ad AS mekteb FROM binalar b
   JOIN mektebler m ON b.mekteb_id=m.id WHERE b.veziyyet='qəza' AND b.aktivdir")
  for (i in seq_len(nrow(qeza)))
    alerts[[length(alerts)+1]] <- list(tip="qeza", seviye="kritik",
      mesaj=sprintf("'%s' — '%s' qəza vəziyyətindədir!", qeza$mekteb[i], qeza$bina[i]))
  alerts
}

# ══════════════════════════════════════════════════════════════════════
#  2. CLAUDE AI
# ══════════════════════════════════════════════════════════════════════

SYSTEM_PROMPT <- "Sən Azərbaycan Respublikası Təhsil Nazirliyinin Təmir Tikinti İdarəsinin süni intellektlə işləyən idarəetmə agentisən.

Vəzifələrin:
1. Layihə İdarəetməsi  2. Bina Monitorinqi  3. Büdcə Nəzarəti
4. Kadr İdarəetməsi  5. Material/Təchizat  6. İnspeksiya
7. Hesabat  8. Xəbərdarlıq  9. Qərar Dəstəyi

CAVAB FORMAT QAYDALARI (ÇOX VACİB):
- Cavablarını HƏMİŞƏ HTML formatında ver.
- Başlıqlar üçün <h2> və <h3> istifadə et.
- Siyahılar üçün <ul><li> və ya <ol><li> istifadə et.
- Cədvəllər üçün <table><thead><tr><th> və <tbody><tr><td> istifadə et.
- Vacib sözləri <strong> ilə, izahları <em> ilə qeyd et.
- Xəbərdarlıqları <blockquote> içində ver.
- Paraqrafları <p> ilə ayır.
- Bölmələri <hr> ilə ayır.
- Rəqəmləri <strong> ilə vurğula.

DİGƏR QAYDALAR:
- Həmişə Azərbaycan dilində cavab ver
- Məlumatları bazadan al, uydurma
- Rəqəmləri dəqiq, pul AZN ilə göstər
- Kritik vəziyyətlərdə xəbərdarlıq ver
- Geniş və ətraflı cavab ver"

empty_obj <- setNames(list(), character(0))
no_params <- list(type = "object", properties = empty_obj)

AI_TOOLS <- list(
  list(name="get_dashboard",        description="Ümumi statistika: məktəb, bina, işçi, layihə, büdcə", input_schema=no_params),
  list(name="get_layiheler",        description="Layihələr. Vəziyyət: planlaşdırılır/icra edilir/tamamlanıb/dayandırılıb/ləğv edilib. Prioritet: kritik/yüksək/normal/aşağı.",
       input_schema=list(type="object", properties=list(veziyyet=list(type="string",description="vəziyyət"), prioritet=list(type="string",description="prioritet")))),
  list(name="get_mektebler",        description="Məktəblər. Bölgə: Bakı/Abşeron-Sumqayıt/Gəncə-Daşkəsən/Şəki-Zaqatala/Lənkəran-Astara/Naxçıvan/Aran/Qarabağ.",
       input_schema=list(type="object", properties=list(bolge=list(type="string",description="Bölgə adı")))),
  list(name="get_binalar",          description="Binalar. Vəziyyət: əla/yaxşı/normal/pis/qəza.",
       input_schema=list(type="object", properties=list(veziyyet=list(type="string",description="vəziyyət")))),
  list(name="get_isciler",          description="İşçilər: ad, vəzifə, maaş, bölgə.",           input_schema=no_params),
  list(name="get_materiallar",      description="Materiallar: anbar miqdarı, minimum hədd.",     input_schema=no_params),
  list(name="get_podratcilar",      description="Podratçılar: ad, VÖEN, ixtisas, reytinq.",      input_schema=no_params),
  list(name="get_inspeksiyalar",    description="İnspeksiyalar: tarix, nəticə, bal, tövsiyə.",   input_schema=no_params),
  list(name="get_bolge_statistika", description="Bölgələr üzrə statistika.",                     input_schema=no_params),
  list(name="get_tehcizat",         description="Təchizat: avadanlıq, vəziyyət, zəmanət.",       input_schema=no_params),
  list(name="get_senedler",         description="Sənədlər: müqavilə, akt, hesabat.",             input_schema=no_params),
  list(name="get_bolmeler",         description="Təşkilati struktur: bölmələr, şeflər.",         input_schema=no_params),
  list(name="get_xeberdarlilar",    description="Xəbərdarlıqlar: gecikən layihə, büdcə aşımı, qəza bina.", input_schema=no_params),
  list(name="run_custom_query",     description="Xüsusi SQL SELECT sorğusu.",
       input_schema=list(type="object", properties=list(sql=list(type="string",description="SELECT sorğusu")), required=list("sql")))
)

execute_ai_tool <- function(tool_name, tool_input) {
  tryCatch({
    result <- switch(tool_name,
      "get_dashboard"=get_stats(), "get_layiheler"=get_layiheler(tool_input$veziyyet,tool_input$prioritet),
      "get_mektebler"=get_mektebler(tool_input$bolge), "get_binalar"=get_binalar(tool_input$veziyyet),
      "get_isciler"=get_isciler(), "get_materiallar"=get_materiallar(), "get_podratcilar"=get_podratcilar(),
      "get_inspeksiyalar"=get_inspeksiyalar(), "get_bolge_statistika"=get_bolge_stat(),
      "get_tehcizat"=get_tehcizat(), "get_senedler"=get_senedler(), "get_bolmeler"=get_bolmeler(),
      "get_xeberdarlilar"=get_xeberdarlilar(),
      "run_custom_query"={ sql<-trimws(tool_input$sql)
        if(!grepl("^SELECT",sql,ignore.case=TRUE)) list(error="Yalnız SELECT icazəlidir.")
        else tryCatch(dbGetQuery(pool,sql), error=function(e) list(error=e$message)) },
      list(error=paste("Naməlum:",tool_name)))
    toJSON(result, auto_unbox=TRUE, pretty=FALSE, na="null")
  }, error=function(e) toJSON(list(error=e$message), auto_unbox=TRUE))
}

call_claude <- function(messages, api_key) {
  body <- list(model="claude-sonnet-4-20250514", max_tokens=4096, system=SYSTEM_PROMPT, tools=AI_TOOLS, messages=messages)
  json_body <- toJSON(body, auto_unbox=TRUE, pretty=FALSE, null="null", na="null")
  cat("[AI] Sorgu gonderilir...\n")
  resp <- request("https://api.anthropic.com/v1/messages") |>
    req_headers("x-api-key"=api_key, "anthropic-version"="2023-06-01", "content-type"="application/json") |>
    req_body_raw(json_body, type="application/json") |> req_timeout(120) |>
    req_error(is_error=function(r) FALSE) |> req_perform()
  status <- resp_status(resp); result <- resp_body_json(resp)
  if (status >= 400) { err <- result$error$message %||% paste("HTTP",status); cat("[AI] Xeta:",err,"\n"); stop(err) }
  cat("[AI] Cavab alindi:", result$stop_reason, "\n"); result
}

ai_chat <- function(user_msg, history, api_key) {
  history[[length(history)+1]] <- list(role="user", content=user_msg)
  repeat {
    response <- call_claude(history, api_key)
    if (response$stop_reason == "tool_use") {
      history[[length(history)+1]] <- list(role="assistant", content=response$content)
      tool_results <- list()
      for (block in response$content) {
        if (!is.null(block$type) && block$type == "tool_use") {
          tool_results[[length(tool_results)+1]] <- list(
            type="tool_result", tool_use_id=block$id, content=execute_ai_tool(block$name, block$input))
        }
      }
      history[[length(history)+1]] <- list(role="user", content=tool_results)
    } else {
      text <- ""
      for (block in response$content) { if (!is.null(block$text)) text <- paste0(text, block$text) }
      history[[length(history)+1]] <- list(role="assistant", content=response$content)
      return(list(reply=text, history=history))
    }
  }
}

# ══════════════════════════════════════════════════════════════════════
#  3. SUALLAR SİYAHISI (kateqoriya üzrə)
# ══════════════════════════════════════════════════════════════════════

SUALLAR <- list(
  "Layihe Idareetmesi" = c(
    "Hazırda neçə aktiv layihə var və onların ümumi vəziyyəti necədir?",
    "Gecikən layihələrin siyahısını göstər — hər biri nə qədər gecikib?",
    "Kritik prioritetli layihələr hansılardır və kim məsuldur?",
    "Son 1 ildə tamamlanan layihələrin siyahısını ver.",
    "Hansı layihələr planlaşdırılır amma hələ başlamayıb?",
    "Dayandırılmış layihələr varmı? Səbəbləri nədir?",
    "Layihələri kateqoriya üzrə qruplaşdır — neçə əsaslı təmir, neçə yeni tikinti var?",
    "Ən yüksək tamamlanma faizinə malik aktiv layihələr hansılardır?",
    "Hər layihənin plan müddəti ilə real müddəti arasındakı fərqi hesabla.",
    "Ən bahalı layihələr hansılardır?"
  ),
  "Bina Monitorinqi" = c(
    "Qəza vəziyyətində olan binalar hansılardır? Hansı məktəblərə aiddir?",
    "Pis vəziyyətdə olan binaların siyahısını ver — tikiliş ili və son təmir ili ilə.",
    "Yanğın siqnalizasiyası olmayan binalar hansılardır?",
    "Generatoru olmayan binaların siyahısını göstər.",
    "Ən qədim binalar hansılardır? Tikiliş ilinə görə sırala.",
    "Son 5 ildə təmir olunmamış binalar hansılardır?",
    "Binalar vəziyyətə görə neçə faiz paylanır?",
    "Lifti olan binaların siyahısını ver.",
    "Hər məktəbin neçə binası var və onların ümumi sahəsi nə qədərdir?",
    "İstilik sistemi olmayan binalar hansılardır?"
  ),
  "Budce Nezareti" = c(
    "Büdcə aşımı olan layihələr hansılardır? Aşım məbləği nə qədərdir?",
    "Ümumi plan büdcə ilə real xərc arasındakı fərq nə qədərdir?",
    "Ən bahalı 5 layihəni göstər — plan büdcə və real xərclə birlikdə.",
    "Hər layihəyə ayrılmış büdcənin neçə faizi xərclənib?",
    "Büdcəsinin 50%-dən azını xərcləmiş layihələr hansılardır?",
    "Bölgələr üzrə ümumi büdcə paylanmasını göstər.",
    "Podratçılara edilmiş ödənişlərin cəmi nə qədərdir?",
    "Hansı layihədə büdcə ən səmərəli istifadə olunub?",
    "Bu il üçün planlaşdırılmış ümumi büdcə nə qədərdir?",
    "Qarabağ bölgəsinə ayrılan büdcə digər bölgələrlə müqayisədə necədir?"
  ),
  "Kadr Idareetmesi" = c(
    "İdarədə neçə aktiv işçi var? Vəzifə kateqoriyasına görə göstər.",
    "Hər bölgədə neçə işçi çalışır?",
    "Orta maaş nə qədərdir? Ən yüksək və ən aşağı maaşı göstər.",
    "Mühəndislik kateqoriyasındakı işçilərin siyahısını ver.",
    "Ən təcrübəli işçilər kimlərdir? İşə başlama tarixinə görə sırala.",
    "Hər vəzifənin maaş diapazonu nədir?",
    "Qarabağ bölgəsində çalışan işçilər kimlərdir?",
    "Hansı bölmənin şefi kimdir? Təşkilati strukturu göstər.",
    "İdarə rəhbərliyinin siyahısını ver.",
    "Hər layihənin məsul işçisi kimdir və əlaqə məlumatları nədir?"
  ),
  "Material ve Anbar" = c(
    "Anbarda hansı materiallar minimum həddin altındadır?",
    "Materialların kateqoriya üzrə siyahısını göstər.",
    "Ən bahalı 5 materialı göstər — vahid qiyməti ilə birlikdə.",
    "Anbardakı materialların ümumi dəyəri nə qədərdir?",
    "Hər kateqoriyada neçə növ material var?",
    "Sement, armatur və kərpicin anbar miqdarı nə qədərdir?",
    "Hansı layihə ən çox material sərf edib?",
    "Materiallar arasında ölçü vahidlərini göstər.",
    "Anbar ehtiyatı ilə minimum hədd arasındakı fərqi hesabla.",
    "Təcili sifariş verilməli materiallar hansılardır?"
  ),
  "Inspeksiya" = c(
    "Son inspeksiya nəticələrini göstər — hansı binalar yoxlanılıb?",
    "Ən aşağı bal almış binalar hansılardır?",
    "İnspeksiya vaxtı keçmiş binalar hansılardır?",
    "Hər inspektorun neçə yoxlama keçirdiyini göstər.",
    "İnspeksiyada narahatlıq qeyd olunmuş binalar hansılardır?",
    "Son inspeksiyada verilmiş tövsiyələrin siyahısını ver.",
    "Ümumi balı 50-dən aşağı olan inspeksiyalar hansılardır?",
    "Hansı binalarda hələ heç inspeksiya keçirilməyib?",
    "İnspeksiya nəticələrini qənaətbəxş/qeyri-qənaətbəxş üzrə qruplaşdır.",
    "Son 6 ayda keçirilmiş inspeksiyaların xülasəsini hazırla."
  ),
  "Podratcilar" = c(
    "Podratçıların reytinq üzrə sıralanmış siyahısını göstər.",
    "Ən yüksək reytinqli 3 podratçı kimdir?",
    "Hər podratçının ixtisas sahəsini göstər.",
    "Hansı podratçı hansı layihədə çalışır?",
    "Podratçılara edilmiş ödənişlərin siyahısını ver.",
    "Reytinqi 3-dən aşağı olan podratçılar varmı?",
    "Hər podratçının əlaqə məlumatlarını göstər.",
    "Ən çox layihədə iştirak edən podratçı kimdir?",
    "Yeni podratçı seçimi üçün hansı sahələrdə ehtiyac var?",
    "Podratçıların VÖEN məlumatlarını göstər."
  ),
  "Techizat" = c(
    "Təchizatın ümumi siyahısını kateqoriya üzrə göstər.",
    "Zəmanət müddəti bitmiş avadanlıqlar hansılardır?",
    "Vəziyyəti pis olan təchizat hansılardır?",
    "Hər məktəbdə hansı təchizat var?",
    "Ən bahalı avadanlıqların siyahısını ver.",
    "Son 1 ildə alınmış təchizatı göstər.",
    "Hər kateqoriyada neçə ədəd avadanlıq var?",
    "Təchizatın ümumi dəyəri nə qədərdir?",
    "Zəmanəti yaxın 3 ayda bitəcək avadanlıqlar hansılardır?",
    "Hansı məktəblərdə təchizat çatışmazlığı var?"
  ),
  "Bolge ve Mekteb" = c(
    "Bölgələr üzrə məktəb, bina və layihə sayını göstər.",
    "Ən çox məktəbi olan bölgə hansıdır?",
    "Qarabağ bölgəsindəki məktəblərin siyahısını ver.",
    "Hər rayonda neçə məktəb var?",
    "Ən çox şagirdi olan 10 məktəbi göstər.",
    "Müəllim-şagird nisbəti ən pis olan məktəblər hansılardır?",
    "Bakı bölgəsindəki bütün məktəblərin siyahısını ver.",
    "Hər bölgənin maaş əmsalı nə qədərdir?",
    "Naxçıvan bölgəsindəki layihələr və onların vəziyyəti necədir?",
    "Bölgələri problem bina sayına görə müqayisə et."
  ),
  "Hesabat ve Tehlil" = c(
    "İdarənin ümumi hesabatını hazırla — bütün sahələr üzrə xülasə.",
    "Bütün xəbərdarlıqları göstər — kritik, yüksək, orta səviyyədə.",
    "Sənədlərin siyahısını ver — müqavilələr, aktlar, hesabatlar.",
    "İdarənin təşkilati strukturunu göstər — bölmələr və şeflər.",
    "Risk təhlili apar — ən çox risk olan layihələr hansılardır?",
    "Resurs planlaması üçün tövsiyə ver — harada işçi çatışmır?",
    "Büdcə optimallaşdırması üçün təklif ver — harada qənaət mümkündür?",
    "Gələn rüb üçün fəaliyyət planı təklif et — prioritetlər nədir?",
    "İdarənin güclü və zəif tərəflərini SWOT formatında hazırla.",
    "Ən vacib 5 problemi prioritet sırasında göstər və həll yolları təklif et."
  )
)


# ══════════════════════════════════════════════════════════════════════
#  4. UI
# ══════════════════════════════════════════════════════════════════════

ui <- page_fluid(
  useShinyjs(),
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#0f4c81"),

  tags$head(tags$style(HTML('
    @import url("https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800;900&display=swap");
    * { font-family: "Inter", -apple-system, sans-serif; }
    body { background: #f4f6fa; margin: 0; font-size: 16px; }

    /* ── Header ── */
    .top-header {
      background: linear-gradient(135deg, #0a2540 0%, #0f4c81 50%, #1a73c7 100%);
      color: white; padding: 0 32px; height: 72px;
      display: flex; align-items: center; justify-content: space-between;
      box-shadow: 0 4px 20px rgba(10,37,64,0.3);
    }
    .top-header .brand { display:flex; align-items:center; gap:16px; }
    .top-header .logo-icon {
      width:48px; height:48px; background:rgba(255,255,255,0.12);
      border-radius:12px; display:flex; align-items:center; justify-content:center;
      font-size:26px; backdrop-filter:blur(10px);
    }
    .top-header h1 { font-size:22px; font-weight:700; margin:0; letter-spacing:-0.3px; }
    .top-header .sub { font-size:13px; opacity:0.7; font-weight:400; }
    .top-header .right-section { display:flex; align-items:center; gap:16px; }
    .ai-status {
      background: rgba(255,255,255,0.1); border: 1px solid rgba(255,255,255,0.15);
      padding: 6px 16px; border-radius: 20px; font-size: 14px; font-weight: 500;
      display: flex; align-items: center; gap: 8px;
    }
    .pulse-dot {
      width:9px; height:9px; background:#34d399; border-radius:50%;
      box-shadow: 0 0 6px #34d399;
      animation: glow 2s ease-in-out infinite;
    }
    @keyframes glow { 0%,100%{box-shadow:0 0 4px #34d399} 50%{box-shadow:0 0 12px #34d399,0 0 20px rgba(52,211,153,0.3)} }

    .key-input input {
      background: rgba(255,255,255,0.1) !important; border: 1px solid rgba(255,255,255,0.2) !important;
      color: white !important; border-radius: 8px !important; font-size: 14px !important;
      padding: 6px 14px !important; width: 240px !important; height: 36px !important;
    }
    .key-input input::placeholder { color: rgba(255,255,255,0.5) !important; }

    /* ── Main Layout — resizable splitter ── */
    .main-wrap {
      display:flex; gap:0; padding:22px 28px; min-height:calc(100vh - 72px);
    }
    .panel-left { flex:1; min-width:400px; overflow-y:auto; padding-right:10px; }
    .panel-right { width:480px; min-width:320px; max-width:900px; flex-shrink:0; padding-left:10px; }

    /* Sürüşdürmə çubuğu */
    .splitter {
      width:8px; cursor:col-resize; background:transparent; flex-shrink:0;
      position:relative; z-index:10; transition:background 0.15s;
      display:flex; align-items:center; justify-content:center;
    }
    .splitter:hover, .splitter.active { background:rgba(15,76,129,0.08); border-radius:8px; }
    .splitter::after {
      content:""; width:3px; height:40px; background:#cbd5e1; border-radius:3px;
      transition:background 0.15s, height 0.15s;
    }
    .splitter:hover::after, .splitter.active::after { background:#0f4c81; height:60px; }

    @media(max-width:1100px) {
      .main-wrap { flex-direction:column; }
      .panel-left { padding-right:0; min-width:auto; }
      .panel-right { width:100% !important; min-width:auto; max-width:none; padding-left:0; margin-top:20px; }
      .splitter { display:none; }
    }

    /* ── Stat Cards ── */
    .stats-row { display:grid; grid-template-columns:repeat(4,1fr); gap:14px; margin-bottom:20px; }
    @media(max-width:900px) { .stats-row { grid-template-columns:repeat(2,1fr); } }

    .kpi-card {
      background:white; border-radius:14px; padding:20px 22px;
      box-shadow: 0 1px 4px rgba(0,0,0,0.06), 0 0 0 1px rgba(0,0,0,0.03);
      position: relative; overflow: hidden; transition: transform 0.2s, box-shadow 0.2s;
    }
    .kpi-card:hover { transform:translateY(-3px); box-shadow:0 8px 25px rgba(0,0,0,0.08); }
    .kpi-card::before {
      content:""; position:absolute; top:0; left:0; right:0; height:3px;
      background: var(--accent, #0f4c81);
    }
    .kpi-icon {
      width:42px; height:42px; border-radius:10px;
      display:flex; align-items:center; justify-content:center;
      font-size:22px; margin-bottom:10px;
      background: var(--accent-bg, #e8f0fe); color: var(--accent, #0f4c81);
    }
    .kpi-label { font-size:13px; color:#6b7280; font-weight:600; text-transform:uppercase; letter-spacing:0.6px; }
    .kpi-value { font-size:34px; font-weight:900; color:#111827; margin:2px 0; letter-spacing:-1px; }
    .kpi-sub { font-size:13px; color:#9ca3af; }

    /* ── Alerts ── */
    .alert-panel {
      background:white; border-radius:14px; padding:20px 22px; margin-bottom:20px;
      box-shadow:0 1px 4px rgba(0,0,0,0.06); max-height:280px; overflow-y:auto;
    }
    .alert-title { font-size:17px; font-weight:700; color:#111827; margin-bottom:14px;
      display:flex; align-items:center; gap:8px; padding-bottom:10px; border-bottom:1px solid #f3f4f6; }
    .alert-row {
      display:flex; align-items:flex-start; gap:10px;
      padding:12px 14px; border-radius:10px; margin-bottom:8px; font-size:15px; line-height:1.55;
    }
    .alert-row.kritik { background:linear-gradient(135deg,#fef2f2,#fff5f5); border-left:3px solid #ef4444; }
    .alert-row.yuksek { background:linear-gradient(135deg,#fffbeb,#fefce8); border-left:3px solid #f59e0b; }
    .alert-row.orta   { background:linear-gradient(135deg,#eff6ff,#f0f7ff); border-left:3px solid #3b82f6; }
    .alert-dot { width:8px; height:8px; border-radius:50%; margin-top:7px; flex-shrink:0; }
    .alert-dot.kritik { background:#ef4444; } .alert-dot.yuksek { background:#f59e0b; } .alert-dot.orta { background:#3b82f6; }
    .alert-text { flex:1; color:#374151; }
    .alert-tag {
      font-size:11px; padding:3px 9px; border-radius:6px; font-weight:700;
      text-transform:uppercase; letter-spacing:0.5px; flex-shrink:0; color:white;
    }
    .alert-tag.kritik { background:#ef4444; } .alert-tag.yuksek { background:#f59e0b; }

    .no-alerts { text-align:center; padding:28px; color:#16a34a; font-weight:600; font-size:16px; }

    /* ── Tabs / Data Tables ── */
    .data-panel {
      background:white; border-radius:14px;
      box-shadow:0 1px 4px rgba(0,0,0,0.06);
      overflow:hidden;
    }
    .data-panel .nav-pills { padding:16px 20px 0; gap:5px; flex-wrap:wrap; }
    .data-panel .nav-pills .nav-link {
      font-size:14px; font-weight:600; color:#6b7280; border-radius:8px;
      padding:8px 16px; transition:all 0.15s;
    }
    .data-panel .nav-pills .nav-link.active { background:#0f4c81; color:white; }
    .data-panel .tab-content { padding:18px 20px; }
    .dataTables_wrapper { font-size:15px; }
    .dataTables_wrapper .dataTables_filter input { font-size:15px; padding:6px 12px; }
    .dataTables_wrapper .dataTables_length select { font-size:15px; }
    .dataTables_wrapper .dataTables_info { font-size:14px; }
    .dataTables_wrapper .dataTables_paginate .paginate_button { font-size:14px !important; }
    table.dataTable { font-size:15px !important; }
    table.dataTable thead th {
      background:#f1f5f9 !important; font-weight:700; font-size:13px;
      text-transform:uppercase; letter-spacing:0.5px; color:#475569;
      padding:12px 14px !important;
    }
    table.dataTable tbody td { padding:10px 14px !important; }
    table.dataTable tbody tr:hover { background:#f8fafc !important; }

    /* ── Chat Panel ── */
    .chat-wrap {
      background:white; border-radius:14px;
      box-shadow: 0 2px 16px rgba(0,0,0,0.08), 0 0 0 1px rgba(0,0,0,0.03);
      display:flex; flex-direction:column; height:calc(100vh - 116px);
      position:sticky; top:94px;
      overflow:hidden;
    }
    .chat-top {
      background: linear-gradient(135deg, #0a2540, #0f4c81);
      color:white; padding:18px 22px; border-radius:14px 14px 0 0;
      display:flex; justify-content:space-between; align-items:center;
    }
    .chat-top-title { font-size:18px; font-weight:700; display:flex; align-items:center; gap:10px; }
    .chat-top .btn-reset {
      background:rgba(255,255,255,0.12); border:1px solid rgba(255,255,255,0.15);
      color:white; padding:6px 14px; border-radius:7px; font-size:13px;
      cursor:pointer; font-weight:600; transition:background 0.15s;
    }
    .chat-top .btn-reset:hover { background:rgba(255,255,255,0.2); }

    /* Sual secici */
    .question-picker {
      padding:12px 18px; border-bottom:1px solid #f3f4f6;
      background: #fafbfd;
    }
    .question-picker label { font-size:13px; font-weight:600; color:#6b7280; margin-bottom:5px; display:block; }
    .question-picker select {
      width:100%; padding:9px 14px; border:1.5px solid #e5e7eb;
      border-radius:9px; font-size:15px; color:#374151;
      background:white; cursor:pointer; transition:border 0.15s;
    }
    .question-picker select:focus { border-color:#0f4c81; outline:none; box-shadow:0 0 0 3px rgba(15,76,129,0.1); }
    .question-picker .pick-row { display:flex; gap:8px; margin-top:10px; }
    .question-picker .btn-ask {
      background:#0f4c81; color:white; border:none; padding:9px 22px;
      border-radius:8px; font-size:15px; font-weight:600; cursor:pointer; transition:background 0.15s;
    }
    .question-picker .btn-ask:hover { background:#0a3660; }

    /* Mesajlar */
    .chat-body { flex:1; overflow-y:auto; padding:18px; display:flex; flex-direction:column; gap:12px; }

    .bubble {
      max-width:92%; padding:14px 18px; border-radius:16px;
      font-size:16px; line-height:1.65; word-wrap:break-word;
    }
    .bubble.user {
      align-self:flex-end; white-space:pre-wrap;
      background: linear-gradient(135deg, #0f4c81, #1a73c7);
      color:white; border-bottom-right-radius:4px;
    }
    /* AI cavab — HTML5 formatında render */
    .bubble.ai {
      align-self:flex-start;
      background:#f8fafc; color:#111827; border-bottom-left-radius:4px;
      border:1px solid #e2e8f0;
    }
    .bubble.ai h1,.bubble.ai h2,.bubble.ai h3,.bubble.ai h4 {
      color:#0f4c81; margin:14px 0 8px; font-weight:700;
    }
    .bubble.ai h1 { font-size:20px; border-bottom:2px solid #e2e8f0; padding-bottom:6px; }
    .bubble.ai h2 { font-size:18px; border-bottom:1px solid #e2e8f0; padding-bottom:4px; }
    .bubble.ai h3 { font-size:16px; }
    .bubble.ai p { margin:6px 0; }
    .bubble.ai ul,.bubble.ai ol { margin:6px 0; padding-left:22px; }
    .bubble.ai li { margin:4px 0; }
    .bubble.ai strong { color:#0a2540; }
    .bubble.ai em { color:#475569; }
    .bubble.ai code {
      background:#e2e8f0; padding:2px 6px; border-radius:4px;
      font-family:monospace; font-size:14px; color:#dc2626;
    }
    .bubble.ai pre {
      background:#1e293b; color:#e2e8f0; padding:14px 16px; border-radius:10px;
      overflow-x:auto; font-size:14px; margin:8px 0;
    }
    .bubble.ai pre code { background:none; color:inherit; padding:0; }
    .bubble.ai table {
      width:100%; border-collapse:collapse; margin:10px 0; font-size:14px;
    }
    .bubble.ai table th {
      background:#0f4c81; color:white; padding:10px 12px; text-align:left;
      font-weight:600; font-size:13px; text-transform:uppercase; letter-spacing:0.3px;
    }
    .bubble.ai table td {
      padding:9px 12px; border-bottom:1px solid #e2e8f0;
    }
    .bubble.ai table tr:nth-child(even) { background:#f8fafc; }
    .bubble.ai table tr:hover { background:#eff6ff; }
    .bubble.ai blockquote {
      border-left:4px solid #0f4c81; margin:10px 0; padding:8px 16px;
      background:#eff6ff; color:#1e3a5f; border-radius:0 8px 8px 0;
      font-style:italic;
    }
    .bubble.ai hr { border:none; border-top:1px solid #e2e8f0; margin:12px 0; }

    .bubble.sys {
      align-self:center; white-space:pre-wrap;
      background:linear-gradient(135deg,#fffbeb,#fefce8);
      color:#92400e; font-size:15px; text-align:center; padding:12px 20px;
      border-radius:12px;
    }

    .thinking-bar {
      padding:12px 18px; background:#f9fafb; border-top:1px solid #f3f4f6;
      display:flex; align-items:center; gap:10px; font-size:15px; color:#6b7280;
    }
    .bounce-dots span {
      display:inline-block; width:7px; height:7px; background:#9ca3af;
      border-radius:50%; margin:0 2px; animation:bounce 1.4s infinite;
    }
    .bounce-dots span:nth-child(2) { animation-delay:0.2s; }
    .bounce-dots span:nth-child(3) { animation-delay:0.4s; }
    @keyframes bounce { 0%,60%,100%{transform:translateY(0)} 30%{transform:translateY(-8px)} }

    /* Input */
    .chat-input-bar {
      padding:14px 18px; border-top:1px solid #e5e7eb;
      display:flex; gap:10px; align-items:flex-end; background:#fafbfd;
      border-radius:0 0 14px 14px;
    }
    .chat-input-bar textarea {
      flex:1; border:1.5px solid #e5e7eb; border-radius:10px;
      padding:12px 16px; font-size:16px; resize:none; min-height:48px; max-height:120px;
      font-family:inherit; transition:border 0.15s;
    }
    .chat-input-bar textarea:focus { border-color:#0f4c81; outline:none; box-shadow:0 0 0 3px rgba(15,76,129,0.08); }
    .btn-send {
      background:#0f4c81; color:white; border:none; border-radius:10px;
      padding:12px 22px; font-size:16px; font-weight:700; cursor:pointer;
      transition:all 0.15s; white-space:nowrap;
    }
    .btn-send:hover { background:#0a3660; transform:scale(1.02); }
    .btn-send:disabled { opacity:0.4; cursor:not-allowed; transform:none; }
  '))),

  # ── Header ──
  div(class = "top-header",
    div(class = "brand",
      div(class = "logo-icon", HTML("&#127959;")),
      div(tags$h1("Təmir Tikinti İdarəsi"), div(class = "sub", "Təhsil Nazirliyi — AI İdarəetmə Sistemi"))
    ),
    div(class = "right-section",
      div(class = "ai-status", span(class = "pulse-dot"), "AI Agent Aktiv"),
      div(class = "key-input",
        passwordInput("api_key", label = NULL, placeholder = "API acari...",
                       width = "220px", value = Sys.getenv("ANTHROPIC_API_KEY")))
    )
  ),

  # ── Main ──
  div(class = "main-wrap",

    # ═══ SOL: Dashboard ═══
    div(class = "panel-left",
      # KPI kartları - sıra 1
      div(class = "stats-row",
        div(class="kpi-card", style="--accent:#0f4c81; --accent-bg:#e8f0fe;",
          div(class="kpi-icon", HTML("&#127979;")),
          div(class="kpi-label","Məktəblər"), div(class="kpi-value", textOutput("s1",inline=T)), div(class="kpi-sub","aktiv təhsil müəssisəsi")),
        div(class="kpi-card", style="--accent:#6366f1; --accent-bg:#eef2ff;",
          div(class="kpi-icon", HTML("&#127960;")),
          div(class="kpi-label","Binalar"), div(class="kpi-value", textOutput("s2",inline=T)), div(class="kpi-sub","qeydiyyatda olan bina")),
        div(class="kpi-card", style="--accent:#059669; --accent-bg:#ecfdf5;",
          div(class="kpi-icon", HTML("&#128101;")),
          div(class="kpi-label","İşçilər"), div(class="kpi-value", textOutput("s3",inline=T)), div(class="kpi-sub","aktiv əməkdaş")),
        div(class="kpi-card", style="--accent:#d97706; --accent-bg:#fffbeb;",
          div(class="kpi-icon", HTML("&#128736;")),
          div(class="kpi-label","Aktiv Layihələr"), div(class="kpi-value", textOutput("s4",inline=T)), div(class="kpi-sub","icra prosesində"))
      ),
      # KPI kartları - sıra 2
      div(class = "stats-row",
        div(class="kpi-card", style="--accent:#059669; --accent-bg:#ecfdf5;",
          div(class="kpi-icon", HTML("&#9989;")),
          div(class="kpi-label","Tamamlanan"), div(class="kpi-value", textOutput("s5",inline=T)), div(class="kpi-sub","bitmiş layihə")),
        div(class="kpi-card", style="--accent:#7c3aed; --accent-bg:#f5f3ff;",
          div(class="kpi-icon", HTML("&#128176;")),
          div(class="kpi-label","Ümumi Büdcə"), div(class="kpi-value", textOutput("s6",inline=T)), div(class="kpi-sub","AZN planlaşdırılmış")),
        div(class="kpi-card", style="--accent:#dc2626; --accent-bg:#fef2f2;",
          div(class="kpi-icon", HTML("&#128308;")),
          div(class="kpi-label","Kritik Layihələr"), div(class="kpi-value", textOutput("s7",inline=T)), div(class="kpi-sub","təcili tədbirlər")),
        div(class="kpi-card", style="--accent:#dc2626; --accent-bg:#fef2f2;",
          div(class="kpi-icon", HTML("&#9888;")),
          div(class="kpi-label","Problem Binalar"), div(class="kpi-value", textOutput("s8",inline=T)), div(class="kpi-sub","pis / qəza vəziyyət"))
      ),

      # Xəbərdarlıqlar
      div(class = "alert-panel",
        div(class = "alert-title", HTML("&#9888;&#65039;"), "Xəbərdarlıqlar"),
        uiOutput("alerts_ui")
      ),

      # Data Tabs
      div(class = "data-panel",
        div(style="display:flex;justify-content:flex-end;padding:14px 20px 0;gap:10px;",
          tags$button(class="btn btn-success btn-sm", id="save_excel",
            style="font-size:14px;font-weight:600;border-radius:8px;padding:7px 18px;",
            HTML("&#128202; Excel-ə yüklə"))
        ),
        navset_pill(id = "tabs",
          nav_panel("Layihələr",
            div(style="padding-top:8px;",
              layout_columns(col_widths=c(4,4,4),
                selectInput("fv","Vəziyyət:", c("Hamısı","planlaşdırılır","icra edilir","tamamlanıb","dayandırılıb"), width="100%"),
                selectInput("fp","Prioritet:", c("Hamısı","kritik","yüksək","normal","aşağı"), width="100%"),
                div(style="padding-top:24px;", actionButton("fa","Filtr", class="btn-primary btn-sm"))
              ),
              DTOutput("t_lay") |> withSpinner(color="#0f4c81", type=6)
            )),
          nav_panel("Məktəblər",
            div(style="padding-top:8px;",
              selectInput("fb","Bölgə:", c("Hamısı"), width="240px"),
              DTOutput("t_mek") |> withSpinner(color="#0f4c81", type=6)
            )),
          nav_panel("Binalar",
            div(style="padding-top:8px;",
              selectInput("fbv","Vəziyyət:", c("Hamısı","əla","yaxşı","normal","pis","qəza"), width="200px"),
              DTOutput("t_bin") |> withSpinner(color="#0f4c81", type=6)
            )),
          nav_panel("İşçilər",       div(style="padding-top:8px;", DTOutput("t_isc") |> withSpinner(color="#0f4c81", type=6))),
          nav_panel("Materiallar",   div(style="padding-top:8px;", DTOutput("t_mat") |> withSpinner(color="#0f4c81", type=6))),
          nav_panel("Podratçılar",   div(style="padding-top:8px;", DTOutput("t_pod") |> withSpinner(color="#0f4c81", type=6))),
          nav_panel("İnspeksiya",    div(style="padding-top:8px;", DTOutput("t_ins") |> withSpinner(color="#0f4c81", type=6))),
          nav_panel("Təchizat",      div(style="padding-top:8px;", DTOutput("t_teh") |> withSpinner(color="#0f4c81", type=6))),
          nav_panel("Sənədlər",      div(style="padding-top:8px;", DTOutput("t_sen") |> withSpinner(color="#0f4c81", type=6))),
          nav_panel("Qrafiklər",
            div(style="padding-top:8px;",
              layout_columns(col_widths=c(6,6),
                card(card_header("Bölgələr üzrə məktəb sayı", class="fw-bold"), plotlyOutput("ch1",height="280px")),
                card(card_header("Layihə prioritetləri", class="fw-bold"),       plotlyOutput("ch2",height="280px"))
              ),
              layout_columns(col_widths=c(6,6),
                card(card_header("Büdcə: Plan vs Real", class="fw-bold"),        plotlyOutput("ch3",height="280px")),
                card(card_header("Bina vəziyyətləri", class="fw-bold"),           plotlyOutput("ch4",height="280px"))
              )
            ))
        )
      )
    ),

    # ═══ SPLITTER — sürüşdürmə çubuğu ═══
    div(class = "splitter", id = "splitter"),

    # ═══ SAĞ: AI Chat ═══
    div(class = "panel-right", id = "panel_right",
    div(class = "chat-wrap",
      div(class = "chat-top",
        div(class = "chat-top-title", HTML("&#129302;"), "AI Köməkçi"),
        div(style="display:flex;gap:8px;",
          tags$button(class = "btn-reset", id = "save_chat", HTML("&#128190; Yadda saxla")),
          tags$button(class = "btn-reset", id = "reset_chat", "Yeni söhbət")
        )
      ),

      # Sual seçici
      div(class = "question-picker",
        tags$label("Bölmə üzrə hazır suallar:"),
        selectInput("q_cat", label = NULL,
          choices = c("Bölmə seçin..." = "", names(SUALLAR)), width = "100%"),
        conditionalPanel("input.q_cat !== ''",
          selectInput("q_item", label = NULL, choices = c("Sual seçin..." = ""), width = "100%"),
          conditionalPanel("input.q_item !== ''",
            div(class = "pick-row",
              tags$button(class = "btn-ask", id = "ask_selected", HTML("&#10148; Bu sualı soruş"))
            )
          )
        )
      ),

      # Mesajlar
      div(class = "chat-body", id = "chat_area", uiOutput("chat_ui")),

      hidden(div(id = "thinking_msg",
        div(class = "thinking-bar",
          div(class = "bounce-dots", tags$span(), tags$span(), tags$span()),
          "Düşünürəm, bazadan məlumat alıram..."
        )
      )),

      # Yazı sahəsi
      div(class = "chat-input-bar",
        tags$textarea(id = "chat_input", placeholder = "Sualınızı yazın...", rows = "1"),
        actionButton("send_btn", HTML("Göndər &#10148;"), class = "btn-send")
      )
    ) # chat-wrap
    ) # panel-right
  ), # main-wrap

  tags$script(HTML("
    // Enter ile gonder
    document.getElementById('chat_input').addEventListener('keydown', function(e) {
      if (e.key==='Enter' && !e.shiftKey) {
        e.preventDefault();
        Shiny.setInputValue('chat_enter', this.value, {priority:'event'});
        this.value = '';
      }
    });
    // Auto-resize
    document.getElementById('chat_input').addEventListener('input', function() {
      this.style.height = 'auto';
      this.style.height = Math.min(this.scrollHeight, 100) + 'px';
    });
    Shiny.addCustomMessageHandler('scrollChat', function(m) {
      var el = document.getElementById('chat_area');
      if(el) setTimeout(function(){ el.scrollTop = el.scrollHeight; }, 50);
    });
    Shiny.addCustomMessageHandler('clearInput', function(m) {
      var el = document.getElementById('chat_input');
      if(el) { el.value=''; el.style.height='auto'; }
    });
    // Reset ve Ask duymesi
    document.getElementById('reset_chat').addEventListener('click', function() {
      Shiny.setInputValue('do_reset', Math.random(), {priority:'event'});
    });
    document.getElementById('ask_selected').addEventListener('click', function() {
      Shiny.setInputValue('do_ask', Math.random(), {priority:'event'});
    });
    document.getElementById('save_chat').addEventListener('click', function() {
      Shiny.setInputValue('do_save_chat', Math.random(), {priority:'event'});
    });
    document.getElementById('save_excel').addEventListener('click', function() {
      Shiny.setInputValue('do_save_excel', Math.random(), {priority:'event'});
    });

    // ── Splitter — sürüşdürmə ilə panel eni dəyişir ──
    (function() {
      var splitter = document.getElementById('splitter');
      var panel = document.getElementById('panel_right');
      if (!splitter || !panel) return;
      var dragging = false, startX, startW;

      splitter.addEventListener('mousedown', function(e) {
        dragging = true; startX = e.clientX;
        startW = panel.offsetWidth;
        splitter.classList.add('active');
        document.body.style.cursor = 'col-resize';
        document.body.style.userSelect = 'none';
        e.preventDefault();
      });

      document.addEventListener('mousemove', function(e) {
        if (!dragging) return;
        var diff = startX - e.clientX;
        var newW = Math.max(320, Math.min(startW + diff, window.innerWidth * 0.7));
        panel.style.width = newW + 'px';
      });

      document.addEventListener('mouseup', function() {
        if (dragging) {
          dragging = false;
          splitter.classList.remove('active');
          document.body.style.cursor = '';
          document.body.style.userSelect = '';
        }
      });

      // Touch dəstəyi (tablet/telefon)
      splitter.addEventListener('touchstart', function(e) {
        dragging = true; startX = e.touches[0].clientX;
        startW = panel.offsetWidth;
        splitter.classList.add('active');
      });
      document.addEventListener('touchmove', function(e) {
        if (!dragging) return;
        var diff = startX - e.touches[0].clientX;
        var newW = Math.max(320, Math.min(startW + diff, window.innerWidth * 0.7));
        panel.style.width = newW + 'px';
      });
      document.addEventListener('touchend', function() {
        if (dragging) { dragging = false; splitter.classList.remove('active'); }
      });
    })();
  "))
)


# ══════════════════════════════════════════════════════════════════════
#  5. SERVER
# ══════════════════════════════════════════════════════════════════════

server <- function(input, output, session) {

  # ── Stats ──
  stats <- reactive(get_stats())
  output$s1 <- renderText(stats()$mekteb)
  output$s2 <- renderText(stats()$bina)
  output$s3 <- renderText(stats()$isci)
  output$s4 <- renderText(stats()$aktiv)
  output$s5 <- renderText(stats()$tamamlan)
  output$s6 <- renderText(format(stats()$budce, big.mark=",", scientific=FALSE))
  output$s7 <- renderText(stats()$kritik)
  output$s8 <- renderText(stats()$problem)

  # ── Bölgə doldur ──
  observe({
    bs <- dbGetQuery(pool, "SELECT ad FROM bolgeler WHERE aktivdir ORDER BY ad")$ad
    updateSelectInput(session, "fb", choices = c("Hamısı", bs))
  })

  # ── Sual seçici: kateqoriya dəyişdikdə sualları yenilə ──
  observeEvent(input$q_cat, {
    cat_name <- input$q_cat
    if (is.null(cat_name) || cat_name == "") {
      updateSelectInput(session, "q_item", choices = c("Sual seçin..." = ""))
    } else {
      items <- SUALLAR[[cat_name]]
      updateSelectInput(session, "q_item", choices = c("Sual seçin..." = "", setNames(items, items)))
    }
  })

  # ── Alerts ──
  output$alerts_ui <- renderUI({
    als <- get_xeberdarlilar()
    if (length(als) == 0) return(div(class="no-alerts", HTML("&#9989; Xəbərdarlıq yoxdur. Sistem normaldır.")))
    tags$div(lapply(als, function(a) {
      div(class = paste("alert-row", a$seviye),
        span(class = paste("alert-dot", a$seviye)),
        span(class = "alert-text", a$mesaj),
        span(class = paste("alert-tag", a$seviye), toupper(a$seviye))
      )
    }))
  })

  # ── DataTables ──
  dt <- list(pageLength=10, scrollX=TRUE, language=list(
    search="Axtar:", lengthMenu="_MENU_ sətir",
    info="_START_-_END_ / _TOTAL_", paginate=list(previous="Geri",`next`="İrəli"),
    zeroRecords="Nəticə tapılmadı"))

  output$t_lay <- renderDT({ input$fa; v<-isolate(input$fv); p<-isolate(input$fp)
    datatable(get_layiheler(if(v=="Hamısı")NULL else v, if(p=="Hamısı")NULL else p),
      options=dt, rownames=FALSE,
      colnames=c("ID","Layihə","Vəziyyət","Prioritet","Plan başlama","Plan bitiş","Plan büdcə","Real xərc","Faiz%","Bina","Məktəb","Kateqoriya","Məsul")) })
  output$t_mek <- renderDT({ b<-input$fb; datatable(get_mektebler(if(is.null(b)||b=="Hamısı")NULL else b), options=dt, rownames=FALSE) })
  output$t_bin <- renderDT({ v<-input$fbv; datatable(get_binalar(if(v=="Hamısı")NULL else v), options=dt, rownames=FALSE) })
  output$t_isc <- renderDT(datatable(get_isciler(), options=dt, rownames=FALSE))
  output$t_mat <- renderDT(datatable(get_materiallar(), options=dt, rownames=FALSE))
  output$t_pod <- renderDT(datatable(get_podratcilar(), options=dt, rownames=FALSE))
  output$t_ins <- renderDT(datatable(get_inspeksiyalar(), options=dt, rownames=FALSE))
  output$t_teh <- renderDT(datatable(get_tehcizat(), options=dt, rownames=FALSE))
  output$t_sen <- renderDT(datatable(get_senedler(), options=dt, rownames=FALSE))

  # ── Charts ──
  output$ch1 <- renderPlotly({
    df <- get_bolge_stat()
    plot_ly(df, x=~bolge, y=~mekteb_sayi, type="bar", marker=list(color="#0f4c81",
      line=list(color="#0a3660",width=1))) |>
      layout(xaxis=list(title=""), yaxis=list(title="Say"), margin=list(b=80))
  })
  output$ch2 <- renderPlotly({
    df <- dbGetQuery(pool, "SELECT prioritet, count(*)::int as say FROM layiheler GROUP BY prioritet")
    plot_ly(df, labels=~prioritet, values=~say, type="pie",
      marker=list(colors=c("kritik"="#ef4444","yüksək"="#f59e0b","normal"="#3b82f6","aşağı"="#9ca3af")[df$prioritet]),
      textinfo="label+value") |> layout(showlegend=FALSE)
  })
  output$ch3 <- renderPlotly({
    df <- dbGetQuery(pool, "SELECT ad,plan_budce,real_xerc FROM layiheler WHERE plan_budce>0 ORDER BY plan_budce DESC LIMIT 8")
    plot_ly(df,x=~ad,y=~plan_budce,type="bar",name="Plan",marker=list(color="#0f4c81")) |>
      add_trace(y=~real_xerc,name="Real",marker=list(color="#f59e0b")) |>
      layout(barmode="group",xaxis=list(title="",tickangle=-30),yaxis=list(title="AZN"),
        margin=list(b=120),legend=list(orientation="h"))
  })
  output$ch4 <- renderPlotly({
    df <- dbGetQuery(pool, "SELECT veziyyet, count(*)::int as say FROM binalar WHERE aktivdir GROUP BY veziyyet")
    plot_ly(df,labels=~veziyyet,values=~say,type="pie",
      marker=list(colors=c("əla"="#059669","yaxşı"="#34d399","normal"="#fbbf24","pis"="#f97316","qəza"="#ef4444")[df$veziyyet]),
      textinfo="label+value") |> layout(showlegend=FALSE)
  })

  # ── AI Chat ──
  chat_hist <- reactiveVal(list())
  chat_msgs <- reactiveVal(list(
    list(role="system", text="Salam! Mən Təmir Tikinti İdarəsinin AI köməkçisiyəm.\nBazadakı məlumatları təhlil edib kömək edə bilərəm.\n\nYuxarıda bölmə seçib hazır sual istifadə edə və ya öz sualınızı yaza bilərsiniz.")))

  output$chat_ui <- renderUI({
    tags$div(lapply(chat_msgs(), function(m) {
      cls <- switch(m$role, "user"="bubble user", "assistant"="bubble ai", "bubble sys")
      if (m$role == "assistant") {
        div(class=cls, HTML(m$text))
      } else {
        div(class=cls, m$text)
      }
    }))
  })

  do_send <- function(text) {
    if (is.null(text) || trimws(text) == "") return()
    key <- input$api_key
    if (is.null(key) || trimws(key) == "") {
      ms <- chat_msgs(); ms[[length(ms)+1]] <- list(role="system",text="Yuxarıda API açarı daxil edin.")
      chat_msgs(ms); return()
    }
    ms <- chat_msgs(); ms[[length(ms)+1]] <- list(role="user",text=text); chat_msgs(ms)
    session$sendCustomMessage("clearInput", list())
    session$sendCustomMessage("scrollChat", list())
    shinyjs::show("thinking_msg")
    tryCatch({
      res <- ai_chat(text, chat_hist(), key)
      chat_hist(res$history)
      ms <- chat_msgs(); ms[[length(ms)+1]] <- list(role="assistant",text=res$reply); chat_msgs(ms)
    }, error=function(e) {
      ms <- chat_msgs(); ms[[length(ms)+1]] <- list(role="system",text=paste("Xəta:",e$message)); chat_msgs(ms)
    })
    shinyjs::hide("thinking_msg")
    session$sendCustomMessage("scrollChat", list())
  }

  observeEvent(input$send_btn, { do_send(input$chat_input); session$sendCustomMessage("clearInput",list()) })
  observeEvent(input$chat_enter, { do_send(input$chat_enter) })

  # Hazır sual göndər
  observeEvent(input$do_ask, {
    q <- input$q_item
    if (!is.null(q) && q != "") do_send(q)
  })

  # Reset
  observeEvent(input$do_reset, {
    chat_hist(list())
    chat_msgs(list(list(role="system",text="Söhbət sıfırlandı. Yeni sualınızı yazın.")))
  })

  # ── AI Cavabını HTML5 olaraq yadda saxla ──
  observeEvent(input$do_save_chat, {
    msgs <- chat_msgs()
    ai_msgs <- Filter(function(m) m$role == "assistant", msgs)
    if (length(ai_msgs) == 0) {
      ms <- chat_msgs(); ms[[length(ms)+1]] <- list(role="system", text="Hələ AI cavabı yoxdur.")
      chat_msgs(ms); return()
    }

    dir_path <- file.path(getwd(), "Cavablar")
    if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)

    ts <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
    fname <- file.path(dir_path, paste0("Cavab_", ts, ".html"))

    # Nəfis HTML5 şablon
    html_parts <- c()
    for (m in msgs) {
      if (m$role == "user") {
        html_parts <- c(html_parts, sprintf(
          '<div class="msg user"><div class="msg-label">&#128100; Sual</div><div class="msg-content user-content">%s</div></div>',
          htmltools::htmlEscape(m$text)))
      } else if (m$role == "assistant") {
        html_parts <- c(html_parts, sprintf(
          '<div class="msg ai"><div class="msg-label">&#129302; AI Cavab</div><div class="msg-content ai-content">%s</div></div>',
          m$text))
      }
    }

    html_body <- paste(html_parts, collapse = "\n")

    html_doc <- sprintf('<!DOCTYPE html>
<html lang="az">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>Təmir Tikinti İdarəsi — AI Hesabat (%s)</title>
<style>
  @import url("https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap");
  * { margin:0; padding:0; box-sizing:border-box; }
  body { font-family:"Inter",sans-serif; background:#f4f6fa; color:#1e293b; line-height:1.7; }
  .container { max-width:900px; margin:0 auto; padding:30px 20px; }

  .header {
    background:linear-gradient(135deg,#0a2540,#0f4c81,#1a73c7);
    color:white; padding:30px 40px; border-radius:16px; margin-bottom:30px;
    box-shadow:0 8px 30px rgba(10,37,64,0.3);
  }
  .header h1 { font-size:24px; font-weight:800; margin-bottom:4px; }
  .header .sub { opacity:0.8; font-size:14px; }
  .header .date { margin-top:10px; font-size:13px; opacity:0.7;
    background:rgba(255,255,255,0.1); display:inline-block; padding:4px 14px; border-radius:8px; }

  .msg { margin-bottom:24px; }
  .msg-label { font-size:13px; font-weight:700; color:#6b7280; margin-bottom:6px;
    text-transform:uppercase; letter-spacing:0.5px; }

  .msg-content {
    padding:20px 24px; border-radius:14px;
    box-shadow:0 1px 4px rgba(0,0,0,0.06);
  }
  .user-content {
    background:linear-gradient(135deg,#0f4c81,#1a73c7);
    color:white; font-size:16px; font-weight:500;
  }
  .ai-content {
    background:white; border:1px solid #e2e8f0;
  }
  .ai-content h1,.ai-content h2,.ai-content h3 { color:#0f4c81; margin:16px 0 8px; }
  .ai-content h2 { font-size:20px; border-bottom:2px solid #e2e8f0; padding-bottom:6px; }
  .ai-content h3 { font-size:17px; }
  .ai-content p { margin:8px 0; }
  .ai-content ul,.ai-content ol { padding-left:24px; margin:8px 0; }
  .ai-content li { margin:4px 0; }
  .ai-content strong { color:#0a2540; }
  .ai-content table { width:100%%; border-collapse:collapse; margin:12px 0; font-size:14px; }
  .ai-content table th {
    background:#0f4c81; color:white; padding:10px 14px; text-align:left;
    font-weight:600; font-size:13px; text-transform:uppercase;
  }
  .ai-content table td { padding:9px 14px; border-bottom:1px solid #e2e8f0; }
  .ai-content table tr:nth-child(even) { background:#f8fafc; }
  .ai-content table tr:hover { background:#eff6ff; }
  .ai-content blockquote {
    border-left:4px solid #0f4c81; margin:10px 0; padding:10px 18px;
    background:#eff6ff; border-radius:0 8px 8px 0; font-style:italic;
  }
  .ai-content hr { border:none; border-top:1px solid #e2e8f0; margin:14px 0; }
  .ai-content code { background:#e2e8f0; padding:2px 6px; border-radius:4px; font-size:13px; }

  .footer {
    text-align:center; padding:24px; color:#9ca3af; font-size:13px;
    border-top:1px solid #e2e8f0; margin-top:30px;
  }

  @media print {
    body { background:white; }
    .container { padding:0; max-width:100%%; }
    .header { border-radius:0; box-shadow:none; }
    .msg-content { box-shadow:none; }
    .user-content { background:#0f4c81 !important; -webkit-print-color-adjust:exact; print-color-adjust:exact; }
  }
</style>
</head>
<body>
<div class="container">
  <div class="header">
    <h1>&#127959; Təmir Tikinti İdarəsi</h1>
    <div class="sub">Təhsil Nazirliyi — AI İdarəetmə Sistemi Hesabatı</div>
    <div class="date">&#128197; %s</div>
  </div>
  %s
  <div class="footer">Təmir Tikinti İdarəsi AI Sistemi tərəfindən avtomatik yaradılmışdır.</div>
</div>
</body>
</html>', ts, format(Sys.time(), "%d.%m.%Y  %H:%M"), html_body)

    writeLines(html_doc, fname, useBytes = TRUE)

    ms <- chat_msgs()
    ms[[length(ms)+1]] <- list(role="system",
      text=paste0("Cavab yadda saxlandı:\nCavablar/", basename(fname)))
    chat_msgs(ms)
    session$sendCustomMessage("scrollChat", list())
  })

  # ── Cədvəli Excel-ə yadda saxla ──
  observeEvent(input$do_save_excel, {
    dir_path <- file.path(getwd(), "Excel")
    if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)

    tab <- input$tabs
    ts <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

    tab_map <- list(
      "Layihələr"   = list(fn=function() get_layiheler(NULL,NULL), name="Layiheler"),
      "Məktəblər"   = list(fn=function() get_mektebler(NULL), name="Mektebler"),
      "Binalar"     = list(fn=function() get_binalar(NULL), name="Binalar"),
      "İşçilər"     = list(fn=get_isciler, name="Isciler"),
      "Materiallar" = list(fn=get_materiallar, name="Materiallar"),
      "Podratçılar" = list(fn=get_podratcilar, name="Podratcilar"),
      "İnspeksiya"  = list(fn=get_inspeksiyalar, name="Inspeksiyalar"),
      "Təchizat"    = list(fn=get_tehcizat, name="Techizat"),
      "Sənədlər"    = list(fn=get_senedler, name="Senedler")
    )

    if (is.null(tab) || !(tab %in% names(tab_map))) {
      ms <- chat_msgs(); ms[[length(ms)+1]] <- list(role="system",
        text="Cədvəl tabını seçin (Qrafiklər tab-ı Excel-ə yazılmır).")
      chat_msgs(ms); return()
    }

    info <- tab_map[[tab]]
    df <- info$fn()

    if (nrow(df) == 0) {
      ms <- chat_msgs(); ms[[length(ms)+1]] <- list(role="system", text="Cədvəl boşdur.")
      chat_msgs(ms); return()
    }

    fname <- file.path(dir_path, paste0(info$name, "_", ts, ".xlsx"))

    # Nəfis Excel formatlaşdırma
    wb <- createWorkbook()
    addWorksheet(wb, info$name)

    # Başlıq stili
    hs <- createStyle(
      fontSize = 13, fontColour = "#FFFFFF", fgFill = "#0f4c81",
      halign = "center", valign = "center", textDecoration = "bold",
      border = "TopBottomLeftRight", borderColour = "#0a3660"
    )
    # Məlumat stili
    ds <- createStyle(
      fontSize = 12, halign = "left", valign = "center",
      border = "TopBottomLeftRight", borderColour = "#e2e8f0",
      wrapText = TRUE
    )
    # Cüt sətir stili
    es <- createStyle(fgFill = "#f8fafc")
    # Rəqəm stili
    ns <- createStyle(numFmt = "#,##0.00")

    writeData(wb, info$name, df, startRow = 1, startCol = 1, headerStyle = hs)
    addStyle(wb, info$name, ds, rows = 2:(nrow(df)+1), cols = 1:ncol(df), gridExpand = TRUE)

    # Cüt sətirləri rəngləndirmə
    if (nrow(df) > 1) {
      even_rows <- seq(3, nrow(df)+1, by = 2)
      if (length(even_rows) > 0)
        addStyle(wb, info$name, es, rows = even_rows, cols = 1:ncol(df), gridExpand = TRUE, stack = TRUE)
    }

    # Sütun genişliyi avtomatik
    setColWidths(wb, info$name, cols = 1:ncol(df), widths = "auto")

    # Filtr əlavə et
    addFilter(wb, info$name, row = 1, cols = 1:ncol(df))

    # Səhifəyə lövhə adı
    freezePane(wb, info$name, firstRow = TRUE)

    saveWorkbook(wb, fname, overwrite = TRUE)

    ms <- chat_msgs()
    ms[[length(ms)+1]] <- list(role="system",
      text=paste0(tab, " Excel-ə yazıldı:\nExcel/", basename(fname)))
    chat_msgs(ms)
    session$sendCustomMessage("scrollChat", list())
  })
}


# ══════════════════════════════════════════════════════════════════════
#  6. RUN
# ══════════════════════════════════════════════════════════════════════
shinyApp(ui, server)
