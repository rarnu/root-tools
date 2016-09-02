package com.rarnu.tools.neo.comp;

import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.Color;
import android.util.AttributeSet;
import android.view.Gravity;
import android.widget.LinearLayout;
import android.widget.ProgressBar;
import android.widget.TextView;
import com.rarnu.tools.neo.R;
import com.rarnu.tools.neo.utils.UIUtils;

public class LoadingView extends LinearLayout {

    private ProgressBar pb = null;
    private TextView tv = null;

    public LoadingView(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        initAttrs(attrs);
    }
    public LoadingView(Context context, AttributeSet attrs) {
        super(context, attrs);
        initAttrs(attrs);
    }
    public LoadingView(Context context) {
        super(context);
        initAttrs(null);
    }

    private void initAttrs(AttributeSet attrs) {
        setOrientation(HORIZONTAL);
        setBackground(getContext().getResources().getDrawable(R.drawable.background_layout, getContext().getTheme()));

        // inner component
        pb = new ProgressBar(getContext());
        LinearLayout.LayoutParams rllpPb = new LinearLayout.LayoutParams(UIUtils.dipToPx(36), UIUtils.dipToPx(36));
        rllpPb.setMarginStart(UIUtils.dipToPx(12));
        rllpPb.gravity = Gravity.CENTER_VERTICAL;
        pb.setLayoutParams(rllpPb);

        tv = new TextView(getContext());
        LinearLayout.LayoutParams rllpTv = new LinearLayout.LayoutParams(LinearLayout.LayoutParams.MATCH_PARENT, UIUtils.dipToPx(36));
        rllpTv.setMarginStart(UIUtils.dipToPx(12));
        rllpTv.setMarginEnd(UIUtils.dipToPx(12));
        rllpTv.gravity = Gravity.CENTER_VERTICAL;
        tv.setLayoutParams(rllpTv);
        tv.setGravity(Gravity.CENTER_VERTICAL);
        tv.setTextSize(18f);
        addView(pb);
        addView(tv);

        TypedArray a = getContext().obtainStyledAttributes(attrs, R.styleable.LoadingView, 0, 0);
        tv.setText(a.getString(R.styleable.LoadingView_text));
        tv.setTextColor(a.getColor(R.styleable.LoadingView_textColor, Color.BLACK));
        a.recycle();
    }
}
