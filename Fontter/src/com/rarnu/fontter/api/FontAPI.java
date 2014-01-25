package com.rarnu.fontter.api;

import android.content.Context;
import android.os.Environment;
import com.rarnu.utils.HttpRequest;
import org.apache.http.protocol.HTTP;
import org.json.JSONArray;

import java.io.File;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.List;

public class FontAPI {

    public static final String KEY_CURRENT_FONT = "key_current_font";
    public static String TMP_DIR = Environment.getExternalStorageDirectory().getAbsolutePath() + "/.font_tmp/";

    private static final String BASE_URL = "http://rarnu.7thgen.info/fonts/";
    private static final String TOP_FONTS = BASE_URL + "top.php";
    private static final String SEARCH_FONTS = BASE_URL + "search.php";
    private static final String SEARCH_PARAM = "name=%s";

    public static final String FONTS_DOWNLOAD_URL = BASE_URL + "download/";

    public static List<FontItem> getTopFonts(Context context) {
        String ret = HttpRequest.get(TOP_FONTS, "", HTTP.UTF_8);
        List<FontItem> list = buildFontResult(context, ret);
        return list;
    }

    public static List<FontItem> searchFonts(Context context, String name) {
        try {
            name = URLEncoder.encode(name, HTTP.UTF_8);
        } catch (Exception e) {

        }
        String ret = HttpRequest.get(SEARCH_FONTS, String.format(SEARCH_PARAM, name), HTTP.UTF_8);
        List<FontItem> list = buildFontResult(context, ret);
        return list;
    }

    private static List<FontItem> buildFontResult(Context context, String httpRet) {
        List<FontItem> list = new ArrayList<FontItem>();
        try {
            JSONArray jarr = new JSONArray(httpRet);
            if (jarr != null && jarr.length() != 0) {
                for (int i = 0; i < jarr.length(); i++) {
                    list.add(FontItem.fromJson(context, jarr.getJSONObject(i)));
                }
            }
        } catch (Exception e) {

        }
        return list;
    }
}
