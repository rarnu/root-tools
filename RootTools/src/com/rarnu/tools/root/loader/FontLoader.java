package com.rarnu.tools.root.loader;

import android.content.Context;
import com.rarnu.devlib.base.BaseLoader;
import com.rarnu.tools.root.api.FontAPI;
import com.rarnu.tools.root.common.FontItem;

import java.util.List;

public class FontLoader extends BaseLoader<FontItem> {

    /**
     * 0: top<br />
     * 1: search
     */
    private int mode = 0;
    private String name = "";

    public FontLoader(Context context) {
        super(context);
    }

    public void setMode(int mode) {
        this.mode = mode;
    }

    public void setName(String name) {
        this.name = name;
    }

    @Override
    public List<FontItem> loadInBackground() {
        List<FontItem> list = null;
        if (mode == 0) {
            list = FontAPI.getTopFonts(getContext());
        } else {
            list = FontAPI.searchFonts(getContext(), name);
        }
        return list;
    }
}
