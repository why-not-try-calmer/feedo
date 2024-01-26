const db_name = process.env["TEST"] == "1" ? "feedfarer-test" : "feedfarer"
const username = process.env["MONGO_INITDB_ROOT_USERNAME"]
const password = process.env["MONGO_INITDB_ROOT_PASSWORD"]
new Array(db_name, username, password).forEach(c => console.assert(c))

const conn_str = `mongodb://${username}:${password}@localhost:27017/admin`
const conn = new Mongo(conn_str)
const db = conn.getDB(db_name)
db.items.createIndex({ "i_desc": "text" })

const indexes = db.items.getIndexes()
const found = indexes.find(idx => idx.name == "i_desc_text")
console.assert(found)