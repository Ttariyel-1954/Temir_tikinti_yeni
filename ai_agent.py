"""
Təmir Tikinti İdarəsi - Süni İntellekt Agent Modulu
Claude API ilə işləyən ağıllı idarəetmə agenti
"""
import json
import os
from datetime import date, datetime
from decimal import Decimal

import anthropic
import database as db


def json_serial(obj):
    if isinstance(obj, (date, datetime)):
        return obj.isoformat()
    if isinstance(obj, Decimal):
        return float(obj)
    raise TypeError(f"Type {type(obj)} not serializable")


# ── Agent-in istifadə edəcəyi alətlər ──

TOOLS = [
    {
        "name": "get_dashboard",
        "description": "İdarənin ümumi statistikasını göstərir: məktəb sayı, bina sayı, işçi sayı, aktiv layihələr, büdcə və s.",
        "input_schema": {"type": "object", "properties": {}, "required": []}
    },
    {
        "name": "get_layiheler",
        "description": "Təmir/tikinti layihələrinin siyahısını qaytarır. Vəziyyət və prioritetə görə filtrlənə bilər.",
        "input_schema": {
            "type": "object",
            "properties": {
                "veziyyet": {
                    "type": "string",
                    "description": "Layihə vəziyyəti: planlaşdırılır, icra edilir, tamamlanıb, dayandırılıb, ləğv edilib",
                    "enum": ["planlaşdırılır", "icra edilir", "tamamlanıb", "dayandırılıb", "ləğv edilib"]
                },
                "prioritet": {
                    "type": "string",
                    "description": "Prioritet: kritik, yüksək, normal, aşağı",
                    "enum": ["kritik", "yüksək", "normal", "aşağı"]
                }
            }
        }
    },
    {
        "name": "get_mektebler",
        "description": "Məktəblərin siyahısını qaytarır. Bölgə və ya rayona görə filtrlənə bilər.",
        "input_schema": {
            "type": "object",
            "properties": {
                "bolge_id": {"type": "integer", "description": "Bölgə ID (1=Bakı, 2=Abşeron, 3=Gəncə, 4=Şəki, 5=Lənkəran, 6=Naxçıvan, 7=Aran, 8=Qarabağ)"},
                "rayon_id": {"type": "integer", "description": "Rayon/şəhər ID"}
            }
        }
    },
    {
        "name": "get_binalar",
        "description": "Binaların siyahısını qaytarır. Vəziyyətə və ya məktəbə görə filtrlənə bilər.",
        "input_schema": {
            "type": "object",
            "properties": {
                "veziyyet": {
                    "type": "string",
                    "description": "Bina vəziyyəti: əla, yaxşı, normal, pis, qəza",
                    "enum": ["əla", "yaxşı", "normal", "pis", "qəza"]
                },
                "mekteb_id": {"type": "integer", "description": "Məktəb ID"}
            }
        }
    },
    {
        "name": "get_isciler",
        "description": "İşçilərin siyahısını qaytarır. Bölgə və vəzifəyə görə filtrlənə bilər.",
        "input_schema": {
            "type": "object",
            "properties": {
                "bolge_id": {"type": "integer", "description": "Bölgə ID"},
                "vezife_id": {"type": "integer", "description": "Vəzifə ID"}
            }
        }
    },
    {
        "name": "get_materiallar",
        "description": "Materiallar siyahısını qaytarır. Anbardakı miqdar, minimum hədd və qiymət məlumatları.",
        "input_schema": {"type": "object", "properties": {}, "required": []}
    },
    {
        "name": "get_podratcilar",
        "description": "Podratçı şirkətlərin siyahısını qaytarır.",
        "input_schema": {"type": "object", "properties": {}, "required": []}
    },
    {
        "name": "get_inspeksiyalar",
        "description": "Bina inspeksiya nəticələrini qaytarır.",
        "input_schema": {"type": "object", "properties": {}, "required": []}
    },
    {
        "name": "get_budce_hesabat",
        "description": "Layihələr üzrə büdcə hesabatını qaytarır: plan, xərc, ödənişlər.",
        "input_schema": {"type": "object", "properties": {}, "required": []}
    },
    {
        "name": "get_bolge_statistika",
        "description": "Bölgələr üzrə statistika: məktəb sayı, bina sayı, layihə sayı, büdcə.",
        "input_schema": {"type": "object", "properties": {}, "required": []}
    },
    {
        "name": "get_tehcizat",
        "description": "Təchizat siyahısını qaytarır: avadanlıqlar, miqdar, vəziyyət.",
        "input_schema": {"type": "object", "properties": {}, "required": []}
    },
    {
        "name": "get_senedler",
        "description": "Sənədlərin siyahısını qaytarır: müqavilələr, aktlar, hesabatlar.",
        "input_schema": {"type": "object", "properties": {}, "required": []}
    },
    {
        "name": "get_bolmeler",
        "description": "İdarənin təşkilati strukturunu (bölmələr, şeflər) qaytarır.",
        "input_schema": {"type": "object", "properties": {}, "required": []}
    },
    {
        "name": "get_xeberdarlilar",
        "description": "Sistem xəbərdarlıqları: gecikən layihələr, büdcə aşımı, qəza binaları, material çatışmazlığı.",
        "input_schema": {"type": "object", "properties": {}, "required": []}
    },
    {
        "name": "get_temir_kateqoriyalar",
        "description": "Təmir kateqoriyalarını qaytarır: yeni tikinti, əsaslı təmir, cari təmir və s.",
        "input_schema": {"type": "object", "properties": {}, "required": []}
    },
    {
        "name": "run_custom_query",
        "description": "Xüsusi SQL SELECT sorğusu icra edir. Yalnız SELECT sorğuları icazəlidir.",
        "input_schema": {
            "type": "object",
            "properties": {
                "sql": {"type": "string", "description": "İcra ediləcək SELECT sorğusu"}
            },
            "required": ["sql"]
        }
    }
]


SYSTEM_PROMPT = """Sən Azərbaycan Respublikası Təhsil Nazirliyinin Təmir Tikinti İdarəsinin süni intellektlə işləyən idarəetmə agentisən.

Sənin vəzifələrin:
1. **Layihə İdarəetməsi**: Təmir/tikinti layihələrinin vəziyyətini izləmək, təhlil etmək, tövsiyələr vermək
2. **Bina Monitorinqi**: Məktəb binalarının texniki vəziyyətini qiymətləndirmək, qəza binalarını aşkarlamaq
3. **Büdcə Nəzarəti**: Maliyyə axınını izləmək, büdcə aşımlarını xəbərdarlıq etmək
4. **Kadr İdarəetməsi**: İşçi heyəti haqqında məlumat vermək, resurs planlaması
5. **Material/Təchizat**: Anbar vəziyyəti, material ehtiyatı, təchizat idarəetməsi
6. **İnspeksiya**: Bina yoxlamalarını izləmək, tövsiyələr vermək
7. **Hesabat**: Müxtəlif hesabatlar hazırlamaq, statistika təhlili
8. **Xəbərdarlıq**: Kritik vəziyyətləri aşkarlayıb xəbərdarlıq etmək
9. **Qərar Dəstəyi**: Məlumatlara əsasən ağıllı tövsiyələr vermək

Qaydalar:
- Həmişə Azərbaycan dilində cavab ver
- Məlumatları bazadan al, uydurma
- Rəqəmləri dəqiq göstər
- Pul məbləğlərini AZN ilə göstər
- Tarixləri gün.ay.il formatında göstər
- Cədvəlləri oxunaqlı formatda göstər
- Kritik vəziyyətlərdə dərhal xəbərdarlıq ver
- Tövsiyə verərkən məlumatlara əsaslan
- Geniş və ətraflı cavab ver"""


def execute_tool(tool_name, tool_input):
    """Agent alətini icra edir."""
    if tool_name == "get_dashboard":
        return db.get_dashboard_stats()
    elif tool_name == "get_layiheler":
        return db.get_layiheler(
            veziyyet=tool_input.get("veziyyet"),
            prioritet=tool_input.get("prioritet")
        )
    elif tool_name == "get_mektebler":
        return db.get_mektebler(
            bolge_id=tool_input.get("bolge_id"),
            rayon_id=tool_input.get("rayon_id")
        )
    elif tool_name == "get_binalar":
        return db.get_binalar(
            veziyyet=tool_input.get("veziyyet"),
            mekteb_id=tool_input.get("mekteb_id")
        )
    elif tool_name == "get_isciler":
        return db.get_isciler(
            bolge_id=tool_input.get("bolge_id"),
            vezife_id=tool_input.get("vezife_id")
        )
    elif tool_name == "get_materiallar":
        return db.get_materiallar()
    elif tool_name == "get_podratcilar":
        return db.get_podratcilar()
    elif tool_name == "get_inspeksiyalar":
        return db.get_inspeksiyalar()
    elif tool_name == "get_budce_hesabat":
        return db.get_budce_hesabat()
    elif tool_name == "get_bolge_statistika":
        return db.get_bolge_statistika()
    elif tool_name == "get_tehcizat":
        return db.get_tehcizat()
    elif tool_name == "get_senedler":
        return db.get_senedler()
    elif tool_name == "get_bolmeler":
        return db.get_bolmeler()
    elif tool_name == "get_xeberdarlilar":
        return db.get_xeberdarlilar()
    elif tool_name == "get_temir_kateqoriyalar":
        return db.get_temir_kateqoriyalar()
    elif tool_name == "run_custom_query":
        return db.run_custom_query(tool_input["sql"])
    else:
        return {"error": f"Naməlum alət: {tool_name}"}


def chat(user_message, history=None):
    """
    İstifadəçi mesajını qəbul edir, Claude ilə danışır,
    lazım gəldikdə bazadan məlumat alır, cavab qaytarır.
    """
    client = anthropic.Anthropic()

    messages = history or []
    messages.append({"role": "user", "content": user_message})

    while True:
        response = client.messages.create(
            model="claude-sonnet-4-20250514",
            max_tokens=4096,
            system=SYSTEM_PROMPT,
            tools=TOOLS,
            messages=messages
        )

        # Cavabdakı tool_use bloklarını yoxla
        if response.stop_reason == "tool_use":
            # Əvvəlcə assistant mesajını əlavə et
            messages.append({"role": "assistant", "content": response.content})

            # Hər tool_use üçün nəticə al
            tool_results = []
            for block in response.content:
                if block.type == "tool_use":
                    result = execute_tool(block.name, block.input)
                    tool_results.append({
                        "type": "tool_result",
                        "tool_use_id": block.id,
                        "content": json.dumps(result, default=json_serial, ensure_ascii=False)
                    })

            messages.append({"role": "user", "content": tool_results})
            # Davam et - Claude cavab verənə qədər
        else:
            # Son cavab
            assistant_text = ""
            for block in response.content:
                if hasattr(block, "text"):
                    assistant_text += block.text

            messages.append({"role": "assistant", "content": response.content})
            return assistant_text, messages
