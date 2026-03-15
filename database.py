"""
Təmir Tikinti İdarəsi - Verilənlər Bazası Modulu
PostgreSQL ilə əlaqə və sorğu icra modulu
"""
import psycopg2
import psycopg2.extras
import json
from datetime import date, datetime
from decimal import Decimal


DB_CONFIG = {
    "dbname": "temir_tikinti",
    "host": "localhost",
}


def get_connection():
    return psycopg2.connect(**DB_CONFIG)


def execute_query(sql, params=None, fetch=True):
    """SQL sorğu icra edir və nəticəni dict list kimi qaytarır."""
    conn = get_connection()
    try:
        with conn.cursor(cursor_factory=psycopg2.extras.RealDictCursor) as cur:
            cur.execute(sql, params)
            if fetch:
                rows = cur.fetchall()
                return [dict(row) for row in rows]
            else:
                conn.commit()
                return cur.rowcount
    finally:
        conn.close()


def serialize(obj):
    """JSON serialization üçün köməkçi."""
    if isinstance(obj, (date, datetime)):
        return obj.isoformat()
    if isinstance(obj, Decimal):
        return float(obj)
    return str(obj)


# ──────────────────────────────────────────────
#  Hazır sorğu funksiyaları - Agent bunları çağırır
# ──────────────────────────────────────────────

def get_dashboard_stats():
    """Əsas statistikalar - dashboard üçün."""
    stats = {}

    stats["mekteb_sayi"] = execute_query(
        "SELECT count(*) as say FROM mektebler WHERE aktivdir"
    )[0]["say"]

    stats["bina_sayi"] = execute_query(
        "SELECT count(*) as say FROM binalar WHERE aktivdir"
    )[0]["say"]

    stats["isci_sayi"] = execute_query(
        "SELECT count(*) as say FROM isciler WHERE aktivdir"
    )[0]["say"]

    stats["aktiv_layihe"] = execute_query(
        "SELECT count(*) as say FROM layiheler WHERE veziyyet NOT IN ('tamamlanıb','ləğv edilib')"
    )[0]["say"]

    stats["tamamlanan_layihe"] = execute_query(
        "SELECT count(*) as say FROM layiheler WHERE veziyyet = 'tamamlanıb'"
    )[0]["say"]

    stats["umumi_budce"] = execute_query(
        "SELECT COALESCE(sum(plan_budce),0) as mebleg FROM layiheler"
    )[0]["mebleg"]

    stats["umumi_xerc"] = execute_query(
        "SELECT COALESCE(sum(real_xerc),0) as mebleg FROM layiheler"
    )[0]["mebleg"]

    stats["kritik_layiheler"] = execute_query(
        "SELECT count(*) as say FROM layiheler WHERE prioritet = 'kritik' AND veziyyet != 'tamamlanıb'"
    )[0]["say"]

    stats["pis_veziyyetli_binalar"] = execute_query(
        "SELECT count(*) as say FROM binalar WHERE veziyyet IN ('pis','qəza') AND aktivdir"
    )[0]["say"]

    stats["material_sayi"] = execute_query(
        "SELECT count(*) as say FROM materiallar WHERE aktivdir"
    )[0]["say"]

    stats["az_material"] = execute_query(
        "SELECT count(*) as say FROM materiallar WHERE anbar_sayi <= minimum_say AND aktivdir"
    )[0]["say"]

    return stats


def get_layiheler(veziyyet=None, prioritet=None):
    """Layihələr siyahısı (filtrli)."""
    sql = """
        SELECT l.id, l.ad, l.veziyyet, l.prioritet, l.plan_baslama, l.plan_bitis,
               l.real_baslama, l.real_bitis, l.plan_budce, l.real_xerc,
               l.tamamlanma_faizi,
               b.ad as bina_adi,
               m.ad as mekteb_adi,
               tk.ad as kateqoriya,
               i.ad || ' ' || i.soyad as masul
        FROM layiheler l
        JOIN binalar b ON l.bina_id = b.id
        JOIN mektebler m ON b.mekteb_id = m.id
        JOIN temir_kateqoriyalar tk ON l.kateqoriya_id = tk.id
        LEFT JOIN isciler i ON l.masul_isci_id = i.id
        WHERE 1=1
    """
    params = []
    if veziyyet:
        sql += " AND l.veziyyet = %s"
        params.append(veziyyet)
    if prioritet:
        sql += " AND l.prioritet = %s"
        params.append(prioritet)
    sql += " ORDER BY l.prioritet DESC, l.plan_baslama"
    return execute_query(sql, params)


def get_mektebler(bolge_id=None, rayon_id=None):
    """Məktəblər siyahısı."""
    sql = """
        SELECT m.id, m.ad, m.kod, m.tip, m.unvan, m.sagird_sayi,
               m.muellim_sayi, m.sinif_sayi, m.qarabag_qayidis,
               r.ad as rayon, b.ad as bolge
        FROM mektebler m
        LEFT JOIN rayon_sehirler r ON m.rayon_id = r.id
        LEFT JOIN bolgeler b ON m.bolge_id = b.id
        WHERE m.aktivdir
    """
    params = []
    if bolge_id:
        sql += " AND m.bolge_id = %s"
        params.append(bolge_id)
    if rayon_id:
        sql += " AND m.rayon_id = %s"
        params.append(rayon_id)
    sql += " ORDER BY b.ad, r.ad, m.ad"
    return execute_query(sql, params)


def get_binalar(veziyyet=None, mekteb_id=None):
    """Binaların siyahısı."""
    sql = """
        SELECT b.id, b.ad, b.bina_novu, b.tikilis_ili, b.mertebe_sayi,
               b.umumi_sahe, b.sinif_otaq_sayi, b.veziyyet,
               b.dam_tipi, b.istilik_sistemi, b.son_temir_ili,
               b.son_temir_novu, b.yangin_siqnal, b.generator_var, b.lift_var,
               m.ad as mekteb_adi
        FROM binalar b
        JOIN mektebler m ON b.mekteb_id = m.id
        WHERE b.aktivdir
    """
    params = []
    if veziyyet:
        sql += " AND b.veziyyet = %s"
        params.append(veziyyet)
    if mekteb_id:
        sql += " AND b.mekteb_id = %s"
        params.append(mekteb_id)
    sql += " ORDER BY b.veziyyet DESC, m.ad"
    return execute_query(sql, params)


def get_isciler(bolge_id=None, vezife_id=None):
    """İşçilər siyahısı."""
    sql = """
        SELECT i.id, i.ad, i.soyad, i.ata_adi, i.ise_baslama, i.maas,
               i.telefon, i.email,
               v.ad as vezife, v.kateqoriya,
               b.ad as bolge
        FROM isciler i
        JOIN vezifeler v ON i.vezife_id = v.id
        LEFT JOIN bolgeler b ON i.bolge_id = b.id
        WHERE i.aktivdir
    """
    params = []
    if bolge_id:
        sql += " AND i.bolge_id = %s"
        params.append(bolge_id)
    if vezife_id:
        sql += " AND i.vezife_id = %s"
        params.append(vezife_id)
    sql += " ORDER BY v.kateqoriya, i.soyad"
    return execute_query(sql, params)


def get_materiallar():
    """Materiallar siyahısı."""
    return execute_query("""
        SELECT id, ad, kateqoriya, olcu_vahidi, vahid_qiymeti,
               anbar_sayi, minimum_say,
               CASE WHEN anbar_sayi <= minimum_say THEN true ELSE false END as az_qalıb
        FROM materiallar WHERE aktivdir
        ORDER BY kateqoriya, ad
    """)


def get_podratcilar():
    """Podratçılar siyahısı."""
    return execute_query("""
        SELECT id, ad, voen, telefon, email, ixtisas, reytinq
        FROM podratcilar WHERE aktivdir
        ORDER BY reytinq DESC
    """)


def get_inspeksiyalar():
    """İnspeksiyalar siyahısı."""
    return execute_query("""
        SELECT ins.id, ins.tarix, ins.netice, ins.umumi_bal,
               ins.qeydler, ins.narahatliqlar, ins.tovsiyeler,
               ins.novbeti_tarix,
               b.ad as bina_adi,
               m.ad as mekteb_adi,
               i.ad || ' ' || i.soyad as inspektor
        FROM inspeksiyalar ins
        JOIN binalar b ON ins.bina_id = b.id
        JOIN mektebler m ON b.mekteb_id = m.id
        LEFT JOIN isciler i ON ins.inspektor_id = i.id
        ORDER BY ins.tarix DESC
    """)


def get_budce_hesabat():
    """Büdcə hesabatı."""
    return execute_query("""
        SELECT l.ad as layihe, l.plan_budce, l.real_xerc,
               ROUND(l.real_xerc / NULLIF(l.plan_budce,0) * 100, 1) as xerc_faizi,
               l.tamamlanma_faizi, l.veziyyet,
               COALESCE(SUM(CASE WHEN bu.emeliyyat_tipi = 'ödəniş' THEN bu.meblegi ELSE 0 END), 0) as odenisler,
               COALESCE(SUM(CASE WHEN bu.emeliyyat_tipi = 'ayrılma' THEN bu.meblegi ELSE 0 END), 0) as ayrilma
        FROM layiheler l
        LEFT JOIN budce bu ON l.id = bu.layihe_id
        GROUP BY l.id, l.ad, l.plan_budce, l.real_xerc, l.tamamlanma_faizi, l.veziyyet
        ORDER BY l.plan_budce DESC
    """)


def get_bolge_statistika():
    """Bölgə üzrə statistika."""
    return execute_query("""
        SELECT bo.ad as bolge, bo.emsal,
               count(DISTINCT m.id) as mekteb_sayi,
               count(DISTINCT b.id) as bina_sayi,
               count(DISTINCT l.id) as layihe_sayi,
               COALESCE(SUM(l.plan_budce),0) as umumi_budce,
               count(DISTINCT CASE WHEN b.veziyyet IN ('pis','qəza') THEN b.id END) as problem_bina
        FROM bolgeler bo
        LEFT JOIN mektebler m ON m.bolge_id = bo.id AND m.aktivdir
        LEFT JOIN binalar b ON b.mekteb_id = m.id AND b.aktivdir
        LEFT JOIN layiheler l ON l.bina_id = b.id
        WHERE bo.aktivdir
        GROUP BY bo.id, bo.ad, bo.emsal
        ORDER BY bo.ad
    """)


def get_tehcizat():
    """Təchizat siyahısı."""
    return execute_query("""
        SELECT t.id, t.ad, t.kateqoriya, t.miqdar, t.vahid_qiymeti,
               t.alış_tarixi, t.zemanet_bitis, t.veziyyet,
               m.ad as mekteb_adi
        FROM tehcizat t
        JOIN mektebler m ON t.mekteb_id = m.id
        ORDER BY t.kateqoriya, t.ad
    """)


def get_senedler():
    """Sənədlər siyahısı."""
    return execute_query("""
        SELECT s.id, s.sened_novu, s.ad, s.sened_no, s.tarix,
               l.ad as layihe_adi, m.ad as mekteb_adi
        FROM senedler s
        LEFT JOIN layiheler l ON s.layihe_id = l.id
        LEFT JOIN mektebler m ON s.mekteb_id = m.id
        ORDER BY s.tarix DESC
    """)


def get_bolmeler():
    """Bölmələr (struktur)."""
    return execute_query("""
        SELECT b.id, b.ad, b.kod, b.seviyye,
               p.ad as ust_bolme,
               i.ad || ' ' || i.soyad as sef
        FROM bolmeler b
        LEFT JOIN bolmeler p ON b.ust_id = p.id
        LEFT JOIN isciler i ON b.sef_id = i.id
        WHERE b.aktivdir
        ORDER BY b.seviyye, b.ad
    """)


def get_temir_kateqoriyalar():
    """Təmir kateqoriyaları."""
    return execute_query("SELECT * FROM temir_kateqoriyalar ORDER BY qrup, ad")


def run_custom_query(sql):
    """Yalnız SELECT sorğuları icra edir (təhlükəsizlik)."""
    sql_clean = sql.strip().rstrip(";").strip()
    if not sql_clean.upper().startswith("SELECT"):
        return {"error": "Yalnız SELECT sorğuları icazəlidir."}
    try:
        return execute_query(sql_clean)
    except Exception as e:
        return {"error": str(e)}


def get_xeberdarlilar():
    """Sistem xəbərdarlıqları - kritik vəziyyətləri aşkarlayır."""
    alerts = []

    # Gecikən layihələr
    geciken = execute_query("""
        SELECT l.ad, l.plan_bitis, l.tamamlanma_faizi
        FROM layiheler l
        WHERE l.plan_bitis < CURRENT_DATE
          AND l.veziyyet NOT IN ('tamamlanıb','ləğv edilib')
    """)
    for g in geciken:
        alerts.append({
            "tip": "gecikme",
            "seviye": "kritik",
            "mesaj": f"'{g['ad']}' layihəsi {g['plan_bitis']} tarixində bitməli idi, hələ {g['tamamlanma_faizi']}% tamamlanıb"
        })

    # Büdcə aşımı
    asim = execute_query("""
        SELECT ad, plan_budce, real_xerc
        FROM layiheler
        WHERE real_xerc > plan_budce AND plan_budce > 0
    """)
    for a in asim:
        alerts.append({
            "tip": "budce_asimi",
            "seviye": "yüksək",
            "mesaj": f"'{a['ad']}': büdcə aşımı — plan {a['plan_budce']} AZN, real {a['real_xerc']} AZN"
        })

    # Qəza binaları
    qeza = execute_query("""
        SELECT b.ad as bina, m.ad as mekteb
        FROM binalar b JOIN mektebler m ON b.mekteb_id = m.id
        WHERE b.veziyyet = 'qəza' AND b.aktivdir
    """)
    for q in qeza:
        alerts.append({
            "tip": "qeza_bina",
            "seviye": "kritik",
            "mesaj": f"'{q['mekteb']}' - '{q['bina']}' binası qəza vəziyyətindədir!"
        })

    # Az qalan materiallar
    az = execute_query("""
        SELECT ad, anbar_sayi, minimum_say, olcu_vahidi
        FROM materiallar
        WHERE anbar_sayi <= minimum_say AND aktivdir
    """)
    for m in az:
        alerts.append({
            "tip": "material_azliq",
            "seviye": "orta",
            "mesaj": f"'{m['ad']}': anbarda {m['anbar_sayi']} {m['olcu_vahidi']} qalıb (minimum: {m['minimum_say']})"
        })

    # Vaxtı keçmiş inspeksiyalar
    kecmis = execute_query("""
        SELECT ins.novbeti_tarix, b.ad as bina, m.ad as mekteb
        FROM inspeksiyalar ins
        JOIN binalar b ON ins.bina_id = b.id
        JOIN mektebler m ON b.mekteb_id = m.id
        WHERE ins.novbeti_tarix < CURRENT_DATE
    """)
    for k in kecmis:
        alerts.append({
            "tip": "inspeksiya",
            "seviye": "orta",
            "mesaj": f"'{k['mekteb']}' - '{k['bina']}': inspeksiya vaxtı keçib ({k['novbeti_tarix']})"
        })

    return alerts
