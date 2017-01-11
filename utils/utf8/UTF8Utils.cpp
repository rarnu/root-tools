//
// Created by rarnu on 11/14/16.
//

#include "UTF8Utils.h"

UTF8Utils::UTF8Utils() {
    _str = "";
}

UTF8Utils::UTF8Utils(const string &str) {
    _str = str;
    buildIndex();
}

void UTF8Utils::buildIndex() {
    // mapping index
    utf8map.clear();
    int i = 0, j = 0;
    // filter the head of UTF8
    if (_str[0] == 0xef && _str[1] == 0xbb && _str[2] == 0xbf) {
        i = 3;
    } else if (_str[0] == '\xef' && _str[1] == '\xbb' && _str[2] == '\xbf') {
        i = 3;
    }
    while (_str[i]) {
        if ((_str[i] & 0xc0) != 0x80) {
            utf8map.insert(pair<int, int>(j, i));
            j++;
        }
        i++;
    }
    _length = j;
}

UTF8Utils::~UTF8Utils() {
    utf8map.clear();
}

string UTF8Utils::str() {
    return _str;
}

void UTF8Utils::setStr(const string &s) {
    _str = s;
    buildIndex();
}

int UTF8Utils::length() {
    return _length;
}

string UTF8Utils::charAt(int index) {
    auto iter = utf8map.find(index);
    int widx = 0;
    if (iter != utf8map.end()) {
        widx = iter->second;
    }
    int j = 0;
    string ret = "";
    while (_str[widx]) {
        if ((_str[widx] & 0xc0) != 0x80) {
            j++;
        }
        if (j == 1) {
            ret += _str[widx];
        }
        widx++;
    }
    return ret;
}

string UTF8Utils::replace(int index, const string &replace) {
    string ret = "";
    for (int i = 0; i < _length; i++) {
        string c = charAt(i);
        if (i != index) {
            ret += c;
        } else {
            ret += replace;
        }
    }
    return ret;
}

string UTF8Utils::replaceAll(const string &origin, const string &replace) {
    // replace all
    int pos=0;
    string ret = _str;
    int a = origin.size();
    int b = replace.size();
    while((pos = ret.find(origin, pos)) != -1) {
        ret.replace(pos, a, replace);
        pos += b;
    }
    return ret;
}

string UTF8Utils::buildInnerString(int start, int len) {
    string ret = "";
    for (int i = start; i < start + len; i++) {
        ret += charAt(i);
    }
    return ret;
}

int UTF8Utils::find(const string &s, int offset) {
    int ret = -1;

    UTF8Utils *u8 = new UTF8Utils(s);
    int len = u8->length();
    for (int i = offset; i < _length - len + 1; i++) {
        string innerStr = buildInnerString(i, len);
        if (innerStr == s) {
            ret = i;
            break;
        }
    }
    delete u8;
    return ret;
}

string UTF8Utils::subStringStart(int start) {
    // substring
    string ret = "";
    for (int i = start; i < _length; i++) {
        ret += charAt(i);
    }
    return ret;
}

string UTF8Utils::subStringRange(int start, int end) {
    // substring
    string ret = "";
    for (int i = start; i < end; i++) {
        ret += charAt(i);
    }
    return ret;
}

string UTF8Utils::subStringEnd(int end) {
    // substring
    string ret = "";
    for (int i = 0; i < end; i++) {
        ret += charAt(i);
    }
    return ret;
}

int UTF8Utils::indexOf(const string &s, int offset) {
    return find(s, offset);
}

int UTF8Utils::lastIndexOf(const string &s) {
    int ret = find(s);
    int tmp = ret;
    UTF8Utils *u8 = new UTF8Utils(s);
    while (tmp != -1) {
        tmp = find(s, ret + u8->length());
        if (tmp != -1) {
            ret = tmp;
        }
    }
    delete u8;
    return ret;
}

string UTF8Utils::trim() {
    string ret = _str;
    const string drop[] = {" ", "\n", "\r", "\t"};
    int len = sizeof(drop) / sizeof(*drop);
    for (int i = 0; i < len; i++) {
        ret.erase(ret.find_last_not_of(drop[i]) + 1);
        ret.erase(0, ret.find_first_not_of(drop[i]));
    }
    return ret;
}

bool UTF8Utils::startWith(const string &s) {
    return find(s) == 0;
}

bool UTF8Utils::endWith(const string &s) {
    UTF8Utils *u8 = new UTF8Utils(s);
    int len = u8->length();
    delete u8;
    return lastIndexOf(s) == _length - len;
}

bool UTF8Utils::contains(const string &s) {
    return find(s) != -1;
}

list<string> UTF8Utils::split(const string &s) {
    list<string> lst;
    int pos = 0;
    int len = _str.size();
    int delimLen = s.size();
    if (delimLen == 0) {
        lst.push_back(s);
        return lst;
    }
    while (pos < len) {
        int findPos = _str.find(s, pos);
        if (findPos < 0) {
            lst.push_back(_str.substr(pos, len - pos));
            break;
        }
        lst.push_back(_str.substr(pos, findPos - pos));
        pos = findPos + delimLen;
    }
    return lst;
}

list<string> UTF8Utils::toCharArray() {
    list<string> ret;
    for (int i = 0; i < _length; i++) {
        ret.push_back(charAt(i));
    }
    return ret;
}

bool UTF8Utils::toBool() {
    return toUpper() == "TRUE";
}

int UTF8Utils::toInt() {
    return atoi(_str.c_str());
}

double UTF8Utils::toDouble() {
    // for android 4.4 and lower, atof is not exists.
    return 0.0;
}

float UTF8Utils::toFloat() {
    return (float) toDouble();
}

string UTF8Utils::toUpper() {
    string ret = _str;
    transform(ret.begin(), ret.end(), ret.begin(), (int (*)(int))toupper);
    return ret;
}

string UTF8Utils::toLower() {
    string ret = _str;
    transform(ret.begin(), ret.end(), ret.begin(), (int (*)(int))tolower);
    return ret;
}