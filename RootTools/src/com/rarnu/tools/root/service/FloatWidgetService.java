package com.rarnu.tools.root.service;

import android.util.Log;
import android.view.View;
import android.widget.ImageView;
import android.widget.Toast;
import com.rarnu.devlib.base.BaseFloatService;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.RTConfig;

public class FloatWidgetService extends BaseFloatService {

    ImageView ivLogo;

    @Override
    public int getViewResId() {
        return R.layout.view_float_window;
    }

    @Override
    public void initView(View view) {
        ivLogo = (ImageView) view.findViewById(R.id.ivLogo);
    }

    @Override
    public int getX() {
        return -1;
    }

    @Override
    public int getY() {
        return -1;
    }

    @Override
    public void onPositionChanged(View v, float x, float y) {
        RTConfig.setFloatWindowPosX(this, (int) x);
        RTConfig.setFloatWindowPosY(this, (int) y);
    }

    @Override
    public void onFloatWindowClick() {
        Log.e("FloatWidgetService", "Float Window Click");
        Toast.makeText(this, "Float Window Click", Toast.LENGTH_LONG).show();
    }

    @Override
    public void onFloatWindowLongClick() {
        Log.e("FloatWidgetService", "Float Window Long Click");
        Toast.makeText(this, "Float Window Long Click", Toast.LENGTH_LONG).show();
    }
}
