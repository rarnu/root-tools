package com.rarnu.tools.root.common;

import android.content.Context;
import com.rarnu.tools.root.api.FontAPI;
import com.rarnu.tools.root.utils.DirHelper;
import com.rarnu.utils.ConfigUtils;
import org.json.JSONObject;

import java.io.File;

public class FontItem {
    public String name = "";
    public String fileName = "";
    public String preview = "";
    public boolean inUse = false;
    public boolean isDownloaded = false;

    public static FontItem fromJson(Context context, JSONObject json) {
        FontItem item = null;
        try {
            item = new FontItem();
            item.name = json.getString("name");
            item.fileName = json.getString("file");
            item.preview = json.getString("preview");
            item.inUse = ConfigUtils.getStringConfig(context, FontAPI.KEY_CURRENT_FONT, "").equals(item.name);
            item.isDownloaded = new File(DirHelper.FONT_DIR + item.fileName).exists();
        } catch (Exception e) {

        }
        return item;
    }
}
