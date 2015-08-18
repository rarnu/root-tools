package com.rarnu.devlib.component.zxing.decode;

public final class Intents {
    private Intents() {
    }

    public static final class Scan {

        public static final String ACTION = "com.google.zxing.client.android.SCAN";
        public static final String MODE = "SCAN_MODE";
        public static final String PRODUCT_MODE = "PRODUCT_MODE";
        public static final String ONE_D_MODE = "ONE_D_MODE";
        public static final String QR_CODE_MODE = "QR_CODE_MODE";
        public static final String DATA_MATRIX_MODE = "DATA_MATRIX_MODE";
        public static final String FORMATS = "SCAN_FORMATS";
        public static final String CHARACTER_SET = "CHARACTER_SET";
        public static final String WIDTH = "SCAN_WIDTH";
        public static final String HEIGHT = "SCAN_HEIGHT";
        public static final String RESULT_DISPLAY_DURATION_MS = "RESULT_DISPLAY_DURATION_MS";
        public static final String PROMPT_MESSAGE = "PROMPT_MESSAGE";
        public static final String RESULT = "SCAN_RESULT";
        public static final String RESULT_FORMAT = "SCAN_RESULT_FORMAT";
        public static final String RESULT_BYTES = "SCAN_RESULT_BYTES";
        public static final String RESULT_ORIENTATION = "SCAN_RESULT_ORIENTATION";
        public static final String RESULT_ERROR_CORRECTION_LEVEL = "SCAN_RESULT_ERROR_CORRECTION_LEVEL";
        public static final String RESULT_BYTE_SEGMENTS_PREFIX = "SCAN_RESULT_BYTE_SEGMENTS_";
        public static final String SAVE_HISTORY = "SAVE_HISTORY";

        private Scan() {
        }
    }

    public static final class History {

        public static final String ITEM_NUMBER = "ITEM_NUMBER";

        private History() {
        }
    }

    public static final class Encode {
        public static final String ACTION = "com.google.zxing.client.android.ENCODE";
        public static final String DATA = "ENCODE_DATA";
        public static final String TYPE = "ENCODE_TYPE";
        public static final String FORMAT = "ENCODE_FORMAT";
        public static final String SHOW_CONTENTS = "ENCODE_SHOW_CONTENTS";

        private Encode() {
        }
    }

    public static final class SearchBookContents {
        public static final String ACTION = "com.google.zxing.client.android.SEARCH_BOOK_CONTENTS";
        public static final String ISBN = "ISBN";
        public static final String QUERY = "QUERY";

        private SearchBookContents() {
        }
    }

    public static final class WifiConnect {
        public static final String ACTION = "com.google.zxing.client.android.WIFI_CONNECT";
        public static final String SSID = "SSID";
        public static final String TYPE = "TYPE";
        public static final String PASSWORD = "PASSWORD";

        private WifiConnect() {
        }
    }

    public static final class Share {
        public static final String ACTION = "com.google.zxing.client.android.SHARE";

        private Share() {
        }
    }
}
