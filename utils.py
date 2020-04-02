import json
import csv
import pickle
import sqlalchemy

postgres_connection_string = "postgresql://localhost/econ1425db"
# a mapping that stores unordered pairs
def get_json(path : str):
    with open(path, "r") as f:
        return json.load(f)

def save_json(obj : any, path : str):
    with open(path, "w+") as f:
        json.dump(obj, f)


def save_csv(obj : list, path : str):
    headers = obj[0].keys() 
    with open(path, "w+") as f:
        writer = csv.DictWriter(f, headers)
        writer.writeheader()
        writer.writerows(obj)

def get_csv(path : str):
    with open(path, "r") as csvfile: 
        reader = csv.DictReader(csvfile)
        return list(reader)

def pickle_save(obj, path):
    with open(path, "wb+") as f:
        pickle.dump(obj, f, protocol=3)

def pickle_get(path):
    with open(path, "rb") as f:
        return pickle.load(f)

def db_connect():
    db = sqlalchemy.create_engine(postgres_connection_string)
    engine = db.connect()
    meta = sqlalchemy.MetaData(engine)
    return (db, engine, meta)

def get_bill(engine, bill_id):
    return engine.execute("SELECT * FROM bills WHERE id = '{}'".format(bill_id)).fetchone()[1]

def get_leg(engine, leg_id):
    return engine.execute("SELECT * FROM leg WHERE id = '{}'".format(leg_id)).fetchone()[1]

def get_leg_map(engine):
    return dict(engine.execute("SELECT * FROM leg").fetchall())

def get_bills_map(engine):
    return dict(engine.execute("SELECT * FROM bills").fetchall())

def update(engine, table, leg_id, leg_doc):
    engine.execute(table.update().where(table.c.id == leg_id).values(doc = leg_doc))

def update_all(engine, table, map : dict, verbose = False, increment = 10**4):
    i = 0
    total = len(map)
    for key, value in map.items():
        update(engine, table, key, value)
        if verbose:
            i += 1
            if i % increment == 0:
                print("Finished {} of {}".format(i, total))

class PairMap:
    def __init__(self):
        self.map : dict = {}

    def put(self, a, b, val):
        self.map[self.make_key(a, b)] = val
    
    def get(self, a, b, default = None):
        key = self.make_key(a, b)
        if key in self.map:
            return self.map[key]
        else: 
            return default

    def exists(self, a, b): 
        return self.make_key(a, b) in self.map
    
    def keys(self):
        return [json.loads(s) for s in self.map.keys()]

    def values(self):
        return self.map.values()
    
    def items(self):
        return [(json.loads(key), val) for key, val in self.map.items()]

    def make_key(self, a, b):
        return json.dumps(sorted((a, b)))