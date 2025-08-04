from typing import Dict, Any

from firebase_admin import firestore


class FirestoreDatabase:
    """Firestore database implementation"""

    def __init__(self):
        self.db = firestore.client()

    def update_bot_status(
        self, bot_id: str, status: str, review_data: Dict[str, Any]
    ) -> None:
        """Update bot status in Firestore"""
        # Replace placeholder with actual server timestamp
        if review_data.get("reviewedAt") == "SERVER_TIMESTAMP":
            review_data["reviewedAt"] = firestore.SERVER_TIMESTAMP

        bot_ref = self.db.collection("custom-bots").document(bot_id)
        bot_ref.update(
            {
                "status": status,
                "review": review_data,
                "updatedAt": firestore.SERVER_TIMESTAMP,
            }
        )
