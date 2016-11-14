//
// Created by rarnu on 11/14/16.
//

#ifndef ANDROID_LIBRARY_DICT_NATIVE_UTF8UTILS_H
#define ANDROID_LIBRARY_DICT_NATIVE_UTF8UTILS_H

#include "LocalDictWrapper.h"
#include <map>
#include <list>
#include <algorithm>
#include <cctype>
#include <stdlib.h>

class UTF8Utils {
public:
    UTF8Utils();
    UTF8Utils(const string &str);
    ~UTF8Utils();

    int length();
    string str();
    void setStr(const string &s);
    string charAt(int index);
    string replace(int index, const string &replace);
    string replaceAll(const string &origin, const string &replace);
    int find(const string &s, int offset = 0);
    string subStringStart(int start);
    string subStringRange(int start, int end);
    string subStringEnd(int end);
    int indexOf(const string &s, int offset = 0);
    int lastIndexOf(const string &s);
    string trim();
    bool startWith(const string &s);
    bool endWith(const string &s);
    bool contains(const string &s);
    list<string> split(const string &s);
    list<string> toCharArray();
    bool toBool();
    int toInt();
    double toDouble();
    float toFloat();
    string toUpper();
    string toLower();

private:
    string _str;
    int _length;
    void buildIndex();
    string buildInnerString(int start, int len);
    map<int, int> utf8map;
};


#endif //ANDROID_LIBRARY_DICT_NATIVE_UTF8UTILS_H
