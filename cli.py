#!/usr/bin/env python3
"""
Təmir Tikinti İdarəsi - Terminal Agent
Terminaldan istifadə üçün interaktiv AI köməkçi
"""
import sys
import ai_agent
import database as db


BANNER = """
╔══════════════════════════════════════════════════════════════╗
║     TƏHSIL NAZİRLİYİ - TƏMİR TİKİNTİ İDARƏSİ             ║
║     Süni İntellektlə İdarəetmə Sistemi                     ║
╠══════════════════════════════════════════════════════════════╣
║  Komandalar:                                                ║
║    /status    - Ümumi statistika                            ║
║    /alerts    - Xəbərdarlıqlar                              ║
║    /reset     - Söhbəti sıfırla                             ║
║    /quit      - Çıxış                                       ║
║                                                             ║
║  Hər hansı sual yazın, AI cavab verəcək.                    ║
╚══════════════════════════════════════════════════════════════╝
"""


def show_status():
    stats = db.get_dashboard_stats()
    print("\n┌─── ÜMUMİ STATİSTİKA ───────────────────────┐")
    print(f"│  Məktəblər:        {stats['mekteb_sayi']:>6}                 │")
    print(f"│  Binalar:          {stats['bina_sayi']:>6}                 │")
    print(f"│  İşçilər:          {stats['isci_sayi']:>6}                 │")
    print(f"│  Aktiv layihələr:  {stats['aktiv_layihe']:>6}                 │")
    print(f"│  Tamamlanan:       {stats['tamamlanan_layihe']:>6}                 │")
    print(f"│  Kritik layihələr: {stats['kritik_layiheler']:>6}                 │")
    print(f"│  Problem binalar:  {stats['pis_veziyyetli_binalar']:>6}                 │")
    print(f"│  Ümumi büdcə:  {stats['umumi_budce']:>12,.0f} AZN          │")
    print(f"│  Ümumi xərc:   {stats['umumi_xerc']:>12,.0f} AZN          │")
    print("└─────────────────────────────────────────────┘\n")


def show_alerts():
    alerts = db.get_xeberdarlilar()
    if not alerts:
        print("\n  ✅ Heç bir xəbərdarlıq yoxdur.\n")
        return
    print(f"\n  ⚠  {len(alerts)} xəbərdarlıq var:\n")
    for a in alerts:
        icon = {"kritik": "🔴", "yüksək": "🟠", "orta": "🔵"}.get(a["seviye"], "⚪")
        print(f"  {icon} [{a['seviye'].upper()}] {a['mesaj']}")
    print()


def main():
    print(BANNER)
    show_status()
    show_alerts()

    history = []
    while True:
        try:
            user_input = input("\n🏗  Siz: ").strip()
        except (EOFError, KeyboardInterrupt):
            print("\n\nSağ olun! 👋")
            break

        if not user_input:
            continue

        if user_input == "/quit":
            print("\nSağ olun! 👋")
            break
        elif user_input == "/status":
            show_status()
            continue
        elif user_input == "/alerts":
            show_alerts()
            continue
        elif user_input == "/reset":
            history = []
            print("\n  Söhbət sıfırlandı.\n")
            continue

        print("\n⏳ Düşünürəm...")
        try:
            reply, history = ai_agent.chat(user_input, history)
            print(f"\n🤖 Agent: {reply}")
        except Exception as e:
            print(f"\n❌ Xəta: {e}")
            history = []


if __name__ == "__main__":
    main()
