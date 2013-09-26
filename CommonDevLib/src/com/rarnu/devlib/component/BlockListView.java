package com.rarnu.devlib.component;

import android.content.Context;
import android.util.AttributeSet;
import android.view.ViewGroup;
import android.widget.ListView;

public class BlockListView extends ListView {

    private int itemHeight = 0;

    public BlockListView(Context context, AttributeSet attrs, int defStyle) {
        super(context, attrs, defStyle);
        init();
    }

    public BlockListView(Context context, AttributeSet attrs) {
        super(context, attrs);
        init();
    }

    public BlockListView(Context context) {
        super(context);
        init();
    }

    private void init() {
        setFastScrollEnabled(false);
    }

    public void setItemHeight(int itemHeight) {
        this.itemHeight = itemHeight;
    }

    public void resize() {
        ViewGroup.LayoutParams lp = getLayoutParams();
        lp.height = (itemHeight + getDividerHeight()) * getAdapter().getCount();
        setLayoutParams(lp);
    }
}
