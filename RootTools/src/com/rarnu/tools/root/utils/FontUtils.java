package com.rarnu.tools.root.utils;

import android.os.Environment;
import com.rarnu.tools.root.common.FallbackFontItem;
import com.rarnu.tools.root.common.SystemFontItem;
import com.rarnu.utils.FileUtils;
import org.apache.http.protocol.HTTP;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

public class FontUtils {

    private static final String FONT_PATH = "/system/fonts/";
    private static String[] FALLBACK_FONTS_NAME = new String[]{
            "DroidNaskh-Regular.ttf",
            "DroidNaskh-Regular-SystemUI.ttf",
            "AndroidEmoji.ttf",
            "DroidSansFallback.ttf"
    };
    private static String[] FALLBACK_FONTS_VARIANT = new String[]{
            "elegant",
            "compact",
            "", ""
    };

    private static String[] SYSTEM_FONTS_NAME = new String[]{
            "Roboto-Regular.ttf",
            "Roboto-Light.ttf",
            "Roboto-Thin.ttf",
            "RobotoCondensed-Regular.ttf",
            "DroidSerif-Regular.ttf",
            "DroidSans.ttf",
            "DroidSansMono.ttf"
    };

    private static String[][] SYSTEM_FONTS_FAMILY = new String[][]{
            {"sans-serif", "arial", "helvetica", "tahoma", "verdana"},
            {"sans-serif-light"},
            {"sans-serif-thin"},
            {"sans-serif-condensed"},
            {"serif", "times", "times new roman", "palatino", "georgia", "baskerville", "goudy", "fantasy", "cursive", "ITC Stone Serif"},
            {"Droid Sans"},
            {"monospace", "courier", "courier new", "monaco"}
    };

    public static List<FallbackFontItem> initFallbackFonts() {
        List<FallbackFontItem> list = new ArrayList<FallbackFontItem>();
        for (int i = 0; i < FALLBACK_FONTS_NAME.length; i++) {
            FallbackFontItem item = new FallbackFontItem();
            item.fileName = FALLBACK_FONTS_NAME[i];
            item.variant = FALLBACK_FONTS_VARIANT[i];
            item.exist = new File(FONT_PATH + FALLBACK_FONTS_NAME[i]).exists();
            list.add(item);
        }
        return list;
    }

    public static List<SystemFontItem> initSystemFonts() {
        List<SystemFontItem> list = new ArrayList<SystemFontItem>();
        for (int i = 0; i < SYSTEM_FONTS_NAME.length; i++) {
            SystemFontItem item = new SystemFontItem();
            item.file = SYSTEM_FONTS_NAME[i];
            item.exist = new File(FONT_PATH + SYSTEM_FONTS_NAME[i]).exists();
            for (int j = 0; j < SYSTEM_FONTS_FAMILY[i].length; j++) {
                item.nameset.add(SYSTEM_FONTS_FAMILY[i][j]);
            }
            list.add(item);
        }
        return list;
    }

    private static final String XML_HEAD = "<?xml version=\"1.0\" encoding=\"utf-8\"?>";

    private static String tmpDir = Environment.getExternalStorageDirectory().getAbsolutePath() + "/.font_tmp/";
    public static String FALLBACK_FONTS_XML = "fallback_fonts.xml";
    public static String SYSTEM_FONTS_XML = "system_fonts.xml";

    static {
        File fDir = new File(tmpDir);
        if (!fDir.exists()) {
            fDir.mkdirs();
        }
    }

    public static void saveFallbackFontXml(List<FallbackFontItem> list) {
        StringBuilder sb = new StringBuilder();
        sb.append(XML_HEAD + "\n");
        sb.append("<familyset>\n");
        for (FallbackFontItem item : list) {
            if (item.exist) {
                sb.append("  <family>\n");
                sb.append("    <fileset>\n");
                if (item.variant.equals("")) {
                    sb.append(String.format("      <file>%s</file>\n", item.fileName));
                } else {
                    sb.append(String.format("      <file variant=\"%s\">%s</file>\n", item.variant, item.fileName));
                }
                sb.append("    </fileset>\n");
                sb.append("  </family>\n");
            }
        }
        sb.append("</familyset>\n");
        try {
            FileUtils.rewriteFile(tmpDir + FALLBACK_FONTS_XML, sb.toString(), HTTP.UTF_8);
        } catch (Exception e) {

        }
    }

    public static void saveSystemFontXml(List<SystemFontItem> list) {
        StringBuilder sb = new StringBuilder();
        sb.append(XML_HEAD + "\n");
        sb.append("<familyset>\n");
        for (SystemFontItem item : list) {
            if (item.exist) {
                sb.append("  <family>\n");
                sb.append("    <nameset>\n");
                for (int j = 0; j < item.nameset.size(); j++) {
                    sb.append(String.format("      <name>%s</name>\n", item.nameset.get(j)));
                }
                sb.append("    </nameset>\n");
                sb.append("    <fileset>\n");
                sb.append(String.format("      <file>%s</file>\n", item.file));
                sb.append("    </fileset>\n");
                sb.append("  </family>\n");
            }
        }
        sb.append("</familyset>\n");
        try {
            FileUtils.rewriteFile(tmpDir + SYSTEM_FONTS_XML, sb.toString(), HTTP.UTF_8);
        } catch (Exception e) {

        }
    }
}
