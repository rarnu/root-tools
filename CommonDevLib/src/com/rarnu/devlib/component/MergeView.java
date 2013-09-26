package com.rarnu.devlib.component;

import android.content.Context;
import android.util.AttributeSet;
import android.util.Log;
import android.view.ViewGroup;
import android.widget.RelativeLayout;
import com.rarnu.devlib.component.BlockView.ItemClickListener;

public class MergeView extends RelativeLayout {

    private static final int DEFAULT_WIDTH = ViewGroup.LayoutParams.MATCH_PARENT;
    private static final int DEFAULT_HEIGHT = ViewGroup.LayoutParams.MATCH_PARENT;
    private static final int DEFAULT_COLUMN_COUNT = 5;
    private static final int DEFAULT_ROW_COUNT = 2;
    BlockView[] views = null;
    private int columnCount = DEFAULT_COLUMN_COUNT;
    private int rowCount = DEFAULT_ROW_COUNT;
    private int realWidth = -1;
    private int realHeight = -1;
    private int xSize = -1;
    private int ySize = -1;
    private int padding = 0;

    public MergeView(Context context, AttributeSet attrs, int defStyle) {
        super(context, attrs, defStyle);
        init();
    }

    public MergeView(Context context, AttributeSet attrs) {
        super(context, attrs);
        init();
    }

    public MergeView(Context context) {
        super(context);
        init();
    }

    private void init() {
        setBackgroundColor(0x00000000);
        setSize(DEFAULT_WIDTH, DEFAULT_HEIGHT);
    }

    public void setSize(int width, int height) {
        ViewGroup.LayoutParams vglp = getLayoutParams();
        if (vglp == null) {
            vglp = new ViewGroup.LayoutParams(width, height);
        } else {
            vglp.width = width;
            vglp.height = height;
        }
        realWidth = width;
        realHeight = height;
        setLayoutParams(vglp);
    }

    public void setPadding(int padding) {
        this.padding = padding;
    }

    public void setGridSize(int columnCount, int rowCount) {
        this.columnCount = columnCount;
        this.rowCount = rowCount;
    }

    public void setViews(BlockView[] views, ItemClickListener listener) {
        this.views = views;
        resize(listener);
    }

    private void resize(ItemClickListener listener) {
        if (views == null) {
            return;
        }
        int width = realWidth - (padding * (columnCount + 1));
        int height = realHeight - (padding * (rowCount + 1));

        Log.e("resize", String.format("w:%d h:%d", realWidth, realHeight));

        xSize = width / columnCount;
        ySize = height / rowCount;
        removeAllViews();

        for (BlockView bv : views) {
            RelativeLayout.LayoutParams rllp = new RelativeLayout.LayoutParams(xSize * bv.xsize + ((bv.xsize - 1) * padding), ySize * bv.ysize + ((bv.ysize - 1) * padding));
            if (bv.below != -1) {
                rllp.addRule(RelativeLayout.BELOW, bv.below);
            }
            if (bv.toRightOf != -1) {
                rllp.addRule(RelativeLayout.RIGHT_OF, bv.toRightOf);
            }
            rllp.topMargin = padding;
            rllp.leftMargin = padding;

            bv.setId(bv.id);
            bv.setLayoutParams(rllp);
            bv.setItemClickListener(listener);
            addView(bv);
        }
    }

}
