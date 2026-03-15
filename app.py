"""
Təmir Tikinti İdarəsi - AI İdarəetmə Sistemi
Flask Web Tətbiqi
"""
import json
from datetime import date, datetime
from decimal import Decimal
from flask import Flask, render_template, request, jsonify, session
import database as db
import ai_agent

app = Flask(__name__)
app.secret_key = "temir_tikinti_2026_secret"


def json_serial(obj):
    if isinstance(obj, (date, datetime)):
        return obj.isoformat()
    if isinstance(obj, Decimal):
        return float(obj)
    return str(obj)


@app.route("/")
def index():
    stats = db.get_dashboard_stats()
    alerts = db.get_xeberdarlilar()
    return render_template("index.html", stats=stats, alerts=alerts)


@app.route("/chat", methods=["POST"])
def chat():
    data = request.get_json()
    user_message = data.get("message", "")
    if not user_message.strip():
        return jsonify({"error": "Boş mesaj"}), 400

    history = session.get("chat_history", [])
    try:
        reply, updated_history = ai_agent.chat(user_message, history)
        # Tarixçəni saxla (son 20 mesaj)
        session["chat_history"] = updated_history[-20:]
        return jsonify({"reply": reply})
    except Exception as e:
        return jsonify({"error": str(e)}), 500


@app.route("/reset", methods=["POST"])
def reset_chat():
    session.pop("chat_history", None)
    return jsonify({"status": "ok"})


@app.route("/api/dashboard")
def api_dashboard():
    stats = db.get_dashboard_stats()
    return jsonify(json.loads(json.dumps(stats, default=json_serial)))


@app.route("/api/alerts")
def api_alerts():
    alerts = db.get_xeberdarlilar()
    return jsonify(alerts)


if __name__ == "__main__":
    app.run(debug=True, port=5050)

