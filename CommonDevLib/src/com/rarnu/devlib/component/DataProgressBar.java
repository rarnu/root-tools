package com.rarnu.devlib.component;

import android.content.Context;
import android.util.AttributeSet;
import android.widget.RelativeLayout;
import android.widget.TextView;
import com.rarnu.devlib.R;

public class DataProgressBar extends RelativeLayout {

    TextView tvProgressApp;
    TextView tvProgressCount;

    public DataProgressBar(Context context, AttributeSet attrs, int defStyle) {
        super(context, attrs, defStyle);
        init();
    }

    public DataProgressBar(Context context, AttributeSet attrs) {
        super(context, attrs);
        init();
    }

    public DataProgressBar(Context context) {
        super(context);
        init();
    }

    public void init() {
        addView(inflate(getContext(), R.layout.dataapp_progress, null));
        tvProgressApp = (TextView) findViewById(R.id.tvProgressApp);
        tvProgressCount = (TextView) findViewById(R.id.tvProgressCount);
    }

    public void setAppName(CharSequence text) {
        tvProgressApp.setText(text);
    }

    public void setProgress(CharSequence text) {
        tvProgressCount.setText(text);
    }

}
