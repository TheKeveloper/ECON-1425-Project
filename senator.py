from datetime import datetime
class Senator:
    def __init__(self, src : dict):
        self._id : str = src["id"]["bioguide"]
        self.bioguide_id = src["id"]["bioguide"]
        self.thomas_id = src["id"]["thomas"] if "thomas" in src["id"] else None
        self.first_name = src["name"]["first"]
        self.last_name = src["name"]["last"]
        self.full_name = src["name"]["official_full"]
        self.terms = [{
            "type" : term["type"],
            "start" : datetime.strptime(term["start"], "%Y-%m-%d"),
            "end" : datetime.strptime(term["end"], "%Y-%m-%d"),
            "party" : term["party"],
            "state" : term["state"]
        } for term in src["terms"]]
        self.party = [term for term in self.terms if term["type"] == "sen"][0]["party"]
        self.state = [term for term in self.terms if term["type"] == "sen"][0]["state"]
