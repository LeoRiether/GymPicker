module DB

open System.Data.SQLite

let createDb conn =
    let sql = """
        create table if not exists cache (
            validatorKey text not null,
            gymId integer not null,
            valid integer not null
        );

        create index if not exists cache_index_key_gymId
        on cache (gymId, validatorKey);
    """
    let cmd = new SQLiteCommand(sql, conn)
    cmd.ExecuteNonQuery()

// Setup connection and create database
let conn =
    let dbFilename = "cache.sqlite"
    let connectionString = sprintf "Data Source=%s;Version=3;" dbFilename

    let conn = new SQLiteConnection(connectionString)
    conn.Open()

    createDb conn |> ignore

    conn

let get validatorKey (Gym.Id id): bool option =
    let sql = """
        select valid
        from cache
        where validatorKey=@validatorKey and gymId=@id;
    """
    let cmd = new SQLiteCommand(sql, conn)
    cmd.Parameters.AddWithValue("@validatorKey", validatorKey) |> ignore
    cmd.Parameters.AddWithValue("@id", id) |> ignore

    cmd.ExecuteScalar()
    |> Option.ofObj
    |> Option.map (fun x -> if x :?> int64 = 0L then false else true)

let put validatorKey (Gym.Id id) value =
    let sql = """
        insert into cache (validatorKey, gymId, valid)
        values (@validatorKey, @id, @valid);
    """

    let cmd = new SQLiteCommand(sql, conn)
    cmd.Parameters.AddWithValue("@validatorKey", validatorKey) |> ignore
    cmd.Parameters.AddWithValue("@id", id) |> ignore
    cmd.Parameters.AddWithValue("@valid", value) |> ignore

    cmd.ExecuteNonQuery() |> ignore

    ()
