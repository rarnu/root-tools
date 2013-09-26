package com.rarnu.devlib.component.tools;

import android.content.Context;
import android.graphics.BitmapFactory;
import android.net.Uri;
import android.view.View;
import android.widget.ImageView;
import android.widget.TextView;
import android.widget.VideoView;
import com.rarnu.devlib.component.BlockView;
import com.rarnu.devlib.component.BlockView.FocusCallback;
import com.rarnu.utils.ImageLoader;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

public class MergePage {

    private int[] pageId = null;
    private int[] pageRight = null;
    private int[] pageBelow = null;
    private int[] pageX = null;
    private int[] pageY = null;
    private int[] pageIndex = null;
    private String[] pageData = null;
    private int count;
    private ImageLoader loader;
    private Context context;

    /**
     * the format of json string is:<br>
     * {"count":5,data:[<br>
     * &nbsp;&nbsp;&nbsp;&nbsp;{"id":1,"below":1,"right":1,"xsize":1,"ysize":1,
     * "extra":"data"},<br>
     * &nbsp;&nbsp;&nbsp;&nbsp;... <br>
     * ]}
     */
    public MergePage(Context context, String json) throws Exception {
        this.context = context;
        loader = new ImageLoader(context);
        JSONObject jObj = new JSONObject(json);
        parseJson(jObj);
    }

    private void parseJson(JSONObject json) throws Exception {
        count = json.getInt("count");
        pageId = new int[count];
        pageBelow = new int[count];
        pageRight = new int[count];
        pageX = new int[count];
        pageY = new int[count];
        pageData = new String[count];
        pageIndex = new int[count];

        JSONArray jArr = json.getJSONArray("data");
        JSONObject jPage = null;
        for (int i = 0; i < count; i++) {
            jPage = jArr.getJSONObject(i);
            pageId[i] = jPage.getInt("id");
            pageBelow[i] = jPage.getInt("below");
            pageRight[i] = jPage.getInt("right");
            pageX[i] = jPage.getInt("xsize");
            pageY[i] = jPage.getInt("ysize");
            pageIndex[i] = jPage.getInt("index");
            pageData[i] = jPage.getString("extra");
        }
    }

    public BlockView[] getViews(Context context, int layout, String[] keys, int[] ids, BitmapFactory.Options option, FocusCallback callback) {
        BlockView[] v = new BlockView[count];
        for (int i = 0; i < count; i++) {
            v[i] = new BlockView(context);
            v[i].loadLayout(layout);
            v[i].id = pageId[i];
            v[i].below = pageBelow[i];
            v[i].toRightOf = pageRight[i];
            v[i].index = pageIndex[i];
            v[i].xsize = pageX[i];
            v[i].ysize = pageY[i];
            v[i].extraData = pageData[i];
            v[i].setFocusCallback(callback);
            parseExtra(v[i], keys, ids, pageData[i], option);
        }
        return v;
    }

    private void parseExtra(View v, String[] keys, int[] ids, String extra, BitmapFactory.Options option) {
        try {
            JSONObject json = new JSONObject(extra);

            for (int i = 0; i < keys.length; i++) {
                View subView = v.findViewById(ids[i]);
                if (subView instanceof ImageView) {
                    String uri = json.getString(keys[i]);
                    if (uri.startsWith("http://") || uri.startsWith("https://")) {
                        // load via network
                        loader.DisplayImage(uri, (ImageView) subView);
                    } else if (uri.startsWith("resource://")) {
                        // load via resource
                        int imgRes = context.getResources().getIdentifier(uri.replace("resource://", ""), "drawable", context.getPackageName());
                        ((ImageView) subView).setImageBitmap(BitmapFactory.decodeResource(context.getResources(), imgRes, option));
                    } else {
                        // load via local file
                        ((ImageView) subView).setImageBitmap(BitmapFactory.decodeFile(uri, option));
                    }
                } else if (subView instanceof VideoView) {
                    String rtspUrl = json.getString(keys[i]);
                    ((VideoView) subView).setVideoURI(Uri.parse(rtspUrl));
                    ((VideoView) subView).start();

                } else if (subView instanceof TextView) {
                    ((TextView) subView).setText(json.getString(keys[i]));
                }
            }

        } catch (JSONException e) {
        }

    }

}
