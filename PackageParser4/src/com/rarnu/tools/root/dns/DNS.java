package com.rarnu.tools.root.dns;

public class DNS {

    public static final int DEFAULT_PORT = 53;
    public static final int TYPE_A = 1, // address
            TYPE_NS = 2, // nameserver
            TYPE_MD = 3, // mail domain
            TYPE_MF = 4, // mail forwarder
            TYPE_CNAME = 5, // canonical name
            TYPE_SOA = 6, // start of authority
            TYPE_MB = 7, // mail box
            TYPE_MG = 8, // mail group
            TYPE_MR = 9, // mail rename
            TYPE_NULL = 10, // null
            TYPE_WKS = 11, // well-known services
            TYPE_PTR = 12, // pointer
            TYPE_HINFO = 13, // host info
            TYPE_MINFO = 14, // mail info
            TYPE_MX = 15, // mail exchanger
            TYPE_TXT = 16, // text
            TYPE_AXFR = 252, // zone transfer request
            TYPE_MAILB = 253, // mailbox request
            TYPE_MAILA = 254, // mail agent request
            TYPE_ANY = 255; // request any
    public static final int CLASS_IN = 1, // internet
            CLASS_CS = 2, // csnet
            CLASS_CH = 3, // chaos
            CLASS_HS = 4, // hesiod
            CLASS_ANY = 255; // request any
    public static final int SHIFT_QUERY = 15, SHIFT_OPCODE = 11, SHIFT_AUTHORITATIVE = 10, SHIFT_TRUNCATED = 9,
            SHIFT_RECURSE_PLEASE = 8, SHIFT_RECURSE_AVAILABLE = 7, SHIFT_RESERVED = 4, SHIFT_RESPONSE_CODE = 0;
    public static final int OPCODE_QUERY = 0, OPCODE_IQUERY = 1, OPCODE_STATUS = 2;
    private static final String[] typeNames = {
            "Address", "NameServer", "MailDomain", "MailForwarder", "CanonicalName",
            "StartOfAuthority", "MailBox", "MailGroup", "MailRename", "Null",
            "WellKnownServices", "Pointer", "HostInfo", "MailInfo", "MailExchanger", "Text"};
    private static final String[] codeNames = {
            "Format error", "Server failure", "Name not known", "Not implemented", "Refused"};

    public static String typeName(int type) {
        return ((type >= 1) && (type <= 16)) ? typeNames[type - 1] : "Unknown";
    }

    public static String codeName(int code) {
        return ((code >= 1) && (code <= 5)) ? codeNames[code - 1] : "Unknown error";
    }
}
