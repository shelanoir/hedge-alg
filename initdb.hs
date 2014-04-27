import Database.HDBC
import Database.HDBC.Sqlite3
initSchema =
        ["PRAGMA recursive_triggers=0",
         "CREATE TABLE hedges (hid Integer primary key, hedge Varchar(30) unique)",
         "CREATE TABLE posl(hid Integer references hedges(hid), pred Integer, primary key (hid))",
         "CREATE TABLE negl(hid Integer references hedges(hid), pred Integer, primary key (hid))",
         "CREATE TABLE posrel (hid1 Integer references hedges(hid), hid2 Integer references hedges(hid), primary key (hid1,hid2))",
         "CREATE TABLE negrel (hid1 Integer references hedges(hid), hid2 Integer references hedges(hid), primary key (hid1,hid2))",

         "CREATE TABLE hstring (sid Integer primary key, hid integer references hedges(hid), tail Integer references hstring(sid))", 
         "CREATE TRIGGER posl_delete \
         \ AFTER DELETE ON posl \
         \ begin \
         \         update posl set pred = pred - 1 \
         \         where posl.pred >= OLD.pred and posl.hid != OLD.hid; \
         \ end;",
         "CREATE TRIGGER posl_insert  \
         \ AFTER INSERT ON posl  \
         \ BEGIN  \
         \   UPDATE posl set pred = pred + 1  \
         \      WHERE posl.pred >= NEW.pred and posl.hid != NEW.hid; \
         \ END;",
         "CREATE TRIGGER negl_delete \
         \ AFTER DELETE ON negl \
         \ begin \
         \         update negl set pred = pred - 1 \
         \         where negl.pred >= OLD.pred and negl.hid != OLD.hid; \
         \ end;",
         "CREATE TRIGGER negl_insert  \
         \ AFTER INSERT ON negl  \
         \ BEGIN  \
         \   UPDATE negl set pred = pred + 1  \
         \      WHERE negl.pred >= NEW.pred and negl.hid != NEW.hid; \
         \ END;",
         "CREATE TABLE literal (lid Integer, lstring Varchar(256), \
         \ sid Integer REFERENCES hstring(sid), truthval Varchar(20), PRIMARY KEY (lid))",
         "CREATE TABLE conjLits (cid Integer REFERENCES conj(cid), \
         \ lid Integer REFERENCES literal(lid), primary key (cid, lid))",
         "CREATE TABLE conj (cid Integer, name Varchar(256) DEFAULT\
         \ \"a conjunctive normal form clause\", PRIMARY KEY (cid))"]
initHValue =
        [
         "INSERT INTO hedges(hid,hedge) VALUES (1,\"Very\")",
         "INSERT INTO hedges(hid,hedge) VALUES (2,\"More\")",
         "INSERT INTO hedges(hid,hedge) VALUES (3,\"Less\")",
         "INSERT INTO hedges(hid,hedge) VALUES (4,\"Possibly\")",
         "INSERT INTO posl(hid,pred) VALUES (2,1)",
         "INSERT INTO posl(hid,pred) VALUES (1,1)",
         "INSERT INTO negl(hid,pred) VALUES (4,1)",
         "INSERT INTO negl(hid,pred) VALUES (3,1)",
         "INSERT INTO posrel(hid1,hid2) VALUES (1,2)",
         "INSERT INTO posrel(hid1,hid2) VALUES (1,3)",
         "INSERT INTO posrel(hid1,hid2) VALUES (2,1)",
         "INSERT INTO posrel(hid1,hid2) VALUES (1,1)",
         "INSERT INTO posrel(hid1,hid2) VALUES (2,2)"
         ]

initCValue = 
       ["INSERT INTO literal (lstring, hedges, truthval)\
         \ VALUES (\"America is behind the Ukraine's crisis\",'More', 'False')",
        "INSERT INTO literal (lstring, hedges, truthval)\
        \ VALUES (\"Russia's intervention does not violate the International Law\",'','False')",
        "INSERT INTO literal (lstring, hedges, truthval)\
        \ VALUES (\"Russia's intervention is justified\", 'Very More', 'True')",
        "INSERT INTO literal (lstring, hedges, truthval)\
        \ VALUES (\"Russia's intervention does not violate the International Law\", 'Less', 'True')",
        "INSERT INTO literal (lstring, hedges, truthval)\
        \ VALUES(\"Russia's intervention is justified\", 'Possibly', 'True')",
        "INSERT INTO literal (lstring, hedges, truthval)\
        \ VALUES (\"America is behind the Ukraine's crisis\",'Possibly', 'True')",
        "INSERT INTO literal (lstring, hedges, truthval)\
        \ VALUES(\"Russia's intervention does not violate the International Law\", 'Very', 'True')",

        "INSERT INTO conj (name) VALUES (\"cnf1\")",
        "INSERT INTO conj (name) VALUES (\"cnf2\")",
        "INSERT INTO conj (name) VALUES (\"cnf3\")",
        "INSERT INTO conj (name) VALUES (\"cnf4\")",
--INSERT INTO literal (lstring, hedges, truthval)
--        VALUES("Russia's intervention is justified", 'Very', 'False'); 
        "INSERT INTO conjLits (cid,lid) SELECT conj.cid, literal.lid \
        \ FROM conj, literal \
        \ WHERE conj.name = 'cnf1' \
        \ AND (literal.lid = 1 OR literal.lid = 2 OR literal.lid = 3)",
        "INSERT INTO conjLits (cid,lid) SELECT cid, lid FROM conj, literal \
        \ WHERE conj.name = 'cnf2' AND (lid = 4 OR lid = 5)",
        " INSERT INTO conjLits (cid,lid) SELECT cid, lid FROM conj, literal \
        \  WHERE conj.name = 'cnf3' AND lid = 6",
        "INSERT INTO conjLits (cid,lid) SELECT cid, lid FROM conj, literal \
        \  WHERE conj.name = 'cnf4' AND lid = 7"]
        
        
bulkQuery dbname ls = do conn <- connectSqlite3 dbname
                         q <- mapM (\x-> run conn x []) ls
                         commit conn
                         disconnect conn
                         return q
                         
