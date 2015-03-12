package com.rarnu.devlib.component;

import android.content.Context;
import android.util.AttributeSet;
import android.widget.GridView;
import android.widget.LinearLayout;
import com.rarnu.devlib.adapter.LightCalendarAdapter;

/**
 * Created by rarnu on 2/5/15.
 */
public class LightCalendarView extends LinearLayout {

    GridView grid;
    LightCalendarAdapter adapter;

    public LightCalendarView(Context context, AttributeSet attrs, int defStyle) {
        super(context, attrs, defStyle);
        init();
    }

    public LightCalendarView(Context context, AttributeSet attrs) {
        super(context, attrs);
        init();
    }

    public LightCalendarView(Context context) {
        super(context);
        init();
    }

    private void init() {
        setOrientation(VERTICAL);
        grid = new GridView(getContext());
        grid.setNumColumns(7);
        grid.setStretchMode(GridView.STRETCH_COLUMN_WIDTH);
        LinearLayout.LayoutParams lllpGrid = new LinearLayout.LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT);
        grid.setLayoutParams(lllpGrid);
        grid.setFocusable(false);
        grid.setClickable(false);
        grid.setEnabled(false);
        grid.setVerticalScrollBarEnabled(false);
        grid.setHorizontalScrollBarEnabled(false);
        addView(grid);
    }

    public void setDate(int year, int month, boolean isMondayFirst) {
        adapter = new LightCalendarAdapter(getContext(), year, month);
        adapter.setMondayFirstDay(isMondayFirst);
        grid.setAdapter(adapter);
    }

    public void setWeekHeight(int h) {
        adapter.setItemHeight(h);
    }
}
