import csv
import os
import os.path
import sqlite3
import shutil

import yugioh_env


class yugioh:
    __DATABASE_NAME = "yugioh.db"
    __TBL_DATA = "YGODATA"
    __TBL_EFFECT = "YGOEFFECT"
    __CSV_DATA = "YGODATA.CSV"
    __CSV_EFFECT = "YGOEFFECT.CSV"

    def __init__(self):
        pass

    def __get_version(self):
        conn = sqlite3.connect(self.__DATABASE_NAME)
        c = conn.cursor()
        c.execute("SELECT ver FROM version")
        row = c.fetchone()
        ver = row[0]
        c.close()
        conn.close()
        return ver

    def __set_version(self, conn, c, ver):
        c.execute("INSERT INTO version (ver) VALUES (%d)" % (int(ver)))
        conn.commit()

    def __import_from_csv(self, conn, c, type):
        cf = open(type, "r")
        reader = csv.reader(cf)
        count = 0
        param_str = "?"
        param_count = 3
        table_name = self.__TBL_EFFECT
        if type == self.__CSV_DATA:
            param_count = 51;
            table_name = self.__TBL_DATA
        for i in range(0, param_count - 1, 1):
            param_str += ",?"
        sql_fmt = "INSERT INTO %s VALUES (%s)" % (table_name, param_str)
        for r in reader:
            for i in range(0, len(r), 1):
                r[i] = r[i].decode("utf-8")
            r.insert(0, r[0])
            c.execute(sql_fmt, (r))
            count += 1
        cf.close()
        conn.commit()
        return count

    def __extract_csv(self, mdb):
        yugioh_env.execmd("mdb-export -H \"%s\" YGODATA > %s" % (mdb, self.__CSV_DATA))
        yugioh_env.execmd("mdb-export -H \"%s\" YGOEFFECT > %s" % (mdb, self.__CSV_EFFECT))


    def __create_database(self):
        if os.path.exists(self.__DATABASE_NAME):
            os.remove(self.__DATABASE_NAME)
        conn = sqlite3.connect(self.__DATABASE_NAME)
        return conn


    def __generate_android_data(self, conn, c):
        c.execute("CREATE TABLE android_metadata(locale text)")
        c.execute("INSERT INTO android_metadata VALUES ('en_US')")
        conn.commit()


    def __generate_tables(self, conn, c):
        c.execute(
            "CREATE TABLE YGODATA ( \
            _id int primary key, \
            CardID int, \
            CardPhAl text, \
            CardCamp text, \
            JPCardName text, \
            SCCardName text, \
            TCCardName text, \
            ENCardName text, \
            ENCardName2 text, \
            JPCardType text, \
            SCCardType text, \
            TCCardType text, \
            ENCardType text, \
            JPDCardType text, \
            SCDCardType text, \
            TCDCardType text, \
            ENDCardType text, \
            JPCardRace text, \
            SCCardRace text, \
            TCCardRace text, \
            ENCardRace text, \
            CardBagNum text, \
            JPCardAttribute text, \
            SCCardAttribute text, \
            TCCardAttribute text, \
            ENCardAttribute text, \
            CardStarNum int, \
            SCCardRare text, \
            TCCardRare text, \
            ENCardRare text, \
            CardAtk int, \
            CardAtk2 text, \
            CardDef int, \
            CardDef2 text, \
            JPCardDepict text, \
            SCCardDepict text, \
            TCCardDepict text, \
            ENCardDepict text, \
            SCCardBan text, \
            TCCardBan text, \
            ENCardBan text, \
            CardIsYKDT int, \
            CardIsTKEN int, \
            CardIsCZN text, \
            CardPass text, \
            CardAdjust text, \
            CardLover int, \
            CardUnion text, \
            CardOnceName text, \
            CardAbbrName text, \
            CardEfficeType text)")

        c.execute("CREATE TABLE YGOEFFECT( \
            _id int primary key, \
            ID int, \
            EFFECT text)")
        c.execute("CREATE TABLE version(ver int primary key)")
        conn.commit()

    def convert(self, mdb, ver, output):
        if os.path.exists(self.__DATABASE_NAME):
            os.remove(self.__DATABASE_NAME)
        self.__extract_csv(mdb)
        conn = self.__create_database()
        c = conn.cursor()
        self.__generate_android_data(conn, c)
        self.__generate_tables(conn, c)
        self.__set_version(conn, c, ver)
        count_data = self.__import_from_csv(conn, c, self.__CSV_DATA)
        count_effect = self.__import_from_csv(conn, c, self.__CSV_EFFECT)
        c.close()
        conn.close()
        os.remove(self.__CSV_DATA)
        os.remove(self.__CSV_EFFECT)
        os.remove(mdb)
        if os.path.exists(self.__DATABASE_NAME):
            print("0")
        else:
            print("1")

    def get_version(self):
        ver = 0
        if os.path.exists(self.__DATABASE_NAME):
            ver = self.__get_version()
        print(ver)
        return ver

