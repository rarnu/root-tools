package com.rarnu.devlib.component;

import android.content.Context;
import android.util.AttributeSet;
import android.view.View;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import com.rarnu.devlib.R;

public class PointBar extends RelativeLayout {

    RelativeLayout layBase;
    ImageView[] iPoint;

    public PointBar(Context context, AttributeSet attrs, int defStyle) {
        super(context, attrs, defStyle);
        init();
    }

    public PointBar(Context context, AttributeSet attrs) {
        super(context, attrs);
        init();
    }

    public PointBar(Context context) {
        super(context);
        init();
    }

    private void init() {
        layBase = (RelativeLayout) inflate(getContext(), R.layout.point_bar, null);
        layBase.setLayoutParams(new RelativeLayout.LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT));
        addView(layBase);

        iPoint = new ImageView[10];
        iPoint[0] = (ImageView) findViewById(R.id.imgP0);
        iPoint[1] = (ImageView) findViewById(R.id.imgP1);
        iPoint[2] = (ImageView) findViewById(R.id.imgP2);
        iPoint[3] = (ImageView) findViewById(R.id.imgP3);
        iPoint[4] = (ImageView) findViewById(R.id.imgP4);
        iPoint[5] = (ImageView) findViewById(R.id.imgP5);
        iPoint[6] = (ImageView) findViewById(R.id.imgP6);
        iPoint[7] = (ImageView) findViewById(R.id.imgP7);
        iPoint[8] = (ImageView) findViewById(R.id.imgP8);
        iPoint[9] = (ImageView) findViewById(R.id.imgP9);
    }

    public void setPoint(int point) {
        for (int i = 0; i < iPoint.length; i++) {
            iPoint[i].setBackgroundResource(i == point ? R.drawable.point_focus : R.drawable.point);
        }
    }

    public void setPointCount(int count) {
        for (int i = 0; i < iPoint.length; i++) {
            iPoint[i].setVisibility(i < count ? View.VISIBLE : View.GONE);
        }
    }

}
