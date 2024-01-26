db_name = process.env["TEST"] == "1" ? "feedfarer-test" : "feedfarer"
username = process.env["MONGO_INITDB_ROOT_USERNAME"]
password = process.env["MONGO_INITDB_ROOT_PASSWORD"]
conn_str = `mongodb://${username}:${password}@localhost:27017/admin`
conn = Mongo(conn_str)
db = conn.getDB(db_name)
db.items.createIndex({ "i_desc": "text" })
console.assert(db.items.getIndexes())