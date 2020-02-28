
class Bill:
    def __init__(self, src : dict):
        self._id : str = src["bill_id"]
        self.bill_type : str = src["bill_type"]
        self.actions : list = src["actions"]
        self.by_request : bool = src["by_request"]
        self.congress : int = int(src["congress"])
        self.short_title : str = src["short_title"]
        self.status : str = src["status"]