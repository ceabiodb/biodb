mybiodb = biodb::Biodb()

massbank = mybiodb$getFactory()$createConn('massbank')

masssqlite = mybiodb$getFactory()$createConn('mass.sqlite', url = '~/massbank.sqlite')
masssqlite$allowEditing()

mybiodb$copyDb(conn.from = massbank, conn.to = masssqlite)

masssqlite$allowWriting()
masssqlite$write()
