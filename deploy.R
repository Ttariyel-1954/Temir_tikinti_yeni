# ══════════════════════════════════════════════════════════════════════
# Shinyapps.io-ya Deploy Skripti
# ══════════════════════════════════════════════════════════════════════
#
# Addımlar:
#
# 1. Shinyapps.io hesabı açın: https://www.shinyapps.io
#
# 2. Token alın: Dashboard → Account → Tokens → Show → Copy
#
# 3. Aşağıdakı sətirləri öz məlumatlarınızla doldurun və icra edin:
#
#    rsconnect::setAccountInfo(
#      name   = "YOUR_ACCOUNT_NAME",
#      token  = "YOUR_TOKEN",
#      secret = "YOUR_SECRET"
#    )
#
# 4. Shinyapps.io-da mühit dəyişənlərini təyin edin (Dashboard → Application → Settings → Environment):
#      ANTHROPIC_API_KEY = sk-ant-...
#      DB_HOST = your-db-host.com
#      DB_PORT = 5432
#      DB_NAME = temir_tikinti
#      DB_USER = your_user
#      DB_PASSWORD = your_password
#
#    QEYD: PostgreSQL bazanız internetdən əlçatan olmalıdır.
#    Məsələn: Supabase, Neon, ElephantSQL, AWS RDS, Railway və s.
#
# 5. Deploy:

rsconnect::deployApp(
  appDir    = ".",
  appName   = "temir-tikinti-ai",
  appTitle  = "Təmir Tikinti İdarəsi - AI Sistemi",
  appFiles  = c("app.R", "global.R", "demo.sqlite"),
  forceUpdate = TRUE
)
