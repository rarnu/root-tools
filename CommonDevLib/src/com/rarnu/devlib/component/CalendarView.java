package com.rarnu.devlib.component;

import android.content.Context;
import android.graphics.Color;
import android.graphics.drawable.ColorDrawable;
import android.os.Handler;
import android.os.Message;
import android.util.AttributeSet;
import android.util.Log;
import android.view.GestureDetector;
import android.view.Gravity;
import android.view.MotionEvent;
import android.view.View;
import android.view.animation.AnimationUtils;
import android.widget.*;
import com.rarnu.devlib.R;
import com.rarnu.devlib.adapter.CalendarAdapter;
import com.rarnu.devlib.intf.CalendarViewCallback;
import com.rarnu.utils.UIUtils;

import java.util.Calendar;

/**
 * Created by rarnu on 2/3/15.
 */
public class CalendarView extends LinearLayout {

    public static final int GESTURE_HORZ = 0;
    public static final int GESTURE_VERT = 1;

    private static String[] weekString = new String[]{"SUN", "MON", "TUE", "WED", "TUR", "FRI", "SAT"};
    private TextView[] tvWeek = new TextView[7];
    private ViewFlipper flipper;
    private GestureDetector gestureDetector;
    private int gestureMethod = 0;
    private CalendarAdapter calV;
    private GridView gridView;
    private int c_year, c_month, c_day;
    private int jumpMonth = 0;
    private int jumpYear = 0;
    private int itemHeight = UIUtils.dipToPx(40);
    private int currentPosition = -1;
    private boolean isMondayFirstDay = false;
    private boolean showLunar = true;

    private int currentDay = 0;

    private CalendarViewCallback callback;
    private CalendarAdapter.CalendarAdapterCallback adapterCallback;

    public int getCalendarViewHeight() {
        int h = UIUtils.dipToPx(20);
        h += UIUtils.dipToPx(1);
        h += (itemHeight + 1) * 6;
        return h;
    }

    public void setShowLunar(boolean b) {
        this.showLunar = b;
        if (calV != null) {
            calV.setShowLunar(b);
        }
    }

    public CalendarView(Context context, AttributeSet attrs, int defStyle) {
        super(context, attrs, defStyle);
        init();
        initCalendar();
    }

    public CalendarView(Context context, AttributeSet attrs) {
        super(context, attrs);
        init();
        initCalendar();
    }

    public CalendarView(Context context) {
        super(context);
        init();
        initCalendar();
    }

    private void init() {

        Calendar c = Calendar.getInstance();
        c_year = c.get(Calendar.YEAR);
        c_month = c.get(Calendar.MONTH) + 1;
        c_day = c.get(Calendar.DAY_OF_MONTH);

        setOrientation(VERTICAL);

        LinearLayout layWeek = new LinearLayout(getContext());
        layWeek.setOrientation(HORIZONTAL);
        layWeek.setLayoutParams(new LayoutParams(LayoutParams.MATCH_PARENT, UIUtils.dipToPx(20)));
        layWeek.setBackgroundColor(getResources().getColor(android.R.color.white));

        LayoutParams lllp = new LayoutParams(0, LayoutParams.MATCH_PARENT);
        lllp.weight = 1;
        lllp.gravity = Gravity.CENTER;

        for (int i = 0; i < weekString.length; i++) {
            tvWeek[i] = new TextView(getContext());
            tvWeek[i].setLayoutParams(lllp);
            tvWeek[i].setGravity(Gravity.CENTER);
            tvWeek[i].setTextSize(12);
            tvWeek[i].setText(weekString[i]);
            tvWeek[i].setTextColor(getResources().getColor((i == (isMondayFirstDay ? 5 : 0) || i == 6) ? android.R.color.holo_blue_light : R.color.black));
            layWeek.addView(tvWeek[i]);
        }

        addView(layWeek);

        View vSplit = new View(getContext());
        vSplit.setLayoutParams(new LayoutParams(LayoutParams.MATCH_PARENT, UIUtils.dipToPx(1)));
        vSplit.setBackgroundColor(getResources().getColor(android.R.color.darker_gray));
        addView(vSplit);

        flipper = new ViewFlipper(getContext());
        flipper.setLayoutParams(new LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.WRAP_CONTENT));
        addView(flipper);

    }

    private void initCalendar() {
        gestureDetector = new GestureDetector(getContext(), new CalendarGestureListener());
        flipper.removeAllViews();
        calV = new CalendarAdapter(getContext(), jumpYear, jumpMonth, c_year, c_month, c_day, null);
        calV.setItemHeight(itemHeight);
        calV.setMondayFirstDay(isMondayFirstDay);
        calV.setShowLunar(showLunar);
        addGridView();
        gridView.setAdapter(calV);
        flipper.addView(gridView, 0);
        if (callback != null) {
            callback.onMonthChanged(this, Integer.parseInt(calV.getShowYear()), Integer.parseInt(calV.getShowMonth()));
        }
    }

    public void setDate(int jumpYear, int jumpMonth, int year, int month, int day, int position) {
        this.jumpMonth = jumpMonth;
        this.jumpYear = jumpYear;
        Log.e("jumpYear", String.valueOf(jumpYear));
        currentPosition = position;
        addGridView();
        calV = new CalendarAdapter(getContext(), jumpMonth, jumpYear, year, month, day, adapterCallback);
        calV.setItemHeight(itemHeight);
        calV.setMondayFirstDay(isMondayFirstDay);
        if (currentPosition != -1) {
            calV.setCurrentSelected(currentPosition);
        }
        calV.setShowLunar(showLunar);
        gridView.setAdapter(calV);
        if (callback != null) {
            callback.onMonthChanged(this, Integer.parseInt(calV.getShowYear()), Integer.parseInt(calV.getShowMonth()));
        }
        flipper.addView(gridView, 1);
        flipper.setDisplayedChild(1);
        flipper.removeViewAt(0);
    }

    public void setGestureMethod(int gestureMethod) {
        this.gestureMethod = gestureMethod;
    }

    public void setCalendarViewCallback(CalendarViewCallback callback) {
        this.callback = callback;
        if (this.callback != null) {
            this.callback.onMonthChanged(this, Integer.parseInt(calV.getShowYear()), Integer.parseInt(calV.getShowMonth()));
        }
    }

    private void addGridView() {
        LayoutParams params = new LayoutParams(AbsListView.LayoutParams.MATCH_PARENT, AbsListView.LayoutParams.MATCH_PARENT);

        gridView = new GridView(getContext());
        gridView.setNumColumns(7);

        gridView.setGravity(Gravity.CENTER_VERTICAL);
        gridView.setSelector(new ColorDrawable(Color.TRANSPARENT));
        gridView.setVerticalSpacing(1);
        gridView.setHorizontalSpacing(1);
        gridView.setOnTouchListener(new OnTouchListener() {

            public boolean onTouch(View v, MotionEvent event) {
                return gestureDetector.onTouchEvent(event);
            }
        });

        gridView.setOnItemClickListener(new AdapterView.OnItemClickListener() {

            @Override
            public void onItemClick(AdapterView<?> arg0, View arg1, int position, long arg3) {
                currentPosition = position;
                String dayNumber = (String) calV.getItem(position);
                String dv = dayNumber.split("\\.")[1];
                String scheduleDay = calV.getDateByClickItem(position).split("\\.")[0];
                String scheduleYear = calV.getShowYear();
                String scheduleMonth = calV.getShowMonth();
                currentDay = Integer.parseInt(scheduleDay);
                if (callback != null) {
                    callback.onDayClicked(CalendarView.this, Integer.parseInt(scheduleYear), Integer.parseInt(scheduleMonth), Integer.parseInt(scheduleDay), dv);
                }
                calV.setCurrentSelected(currentPosition);
            }
        });
        gridView.setLayoutParams(params);
    }

    private class CalendarGestureListener extends GestureDetector.SimpleOnGestureListener {

        @Override
        public boolean onDoubleTap(MotionEvent e) {
            if (callback != null) {
                callback.onCalendarDoubleTap(CalendarView.this);
            }
            return super.onDoubleTap(e);
        }

        @Override
        public boolean onFling(MotionEvent e1, MotionEvent e2, float velocityX, float velocityY) {
            int gvFlag = 0;
            switch (gestureMethod) {
                case GESTURE_HORZ:
                    if (e1.getX() - e2.getX() > 120) {
                        // left
                        enterNextMonth(gvFlag);
                        return true;
                    } else if (e1.getX() - e2.getX() < -120) {
                        // right
                        enterPrevMonth(gvFlag);
                        return true;
                    }
                    break;
                case GESTURE_VERT:
                    if (e1.getY() - e2.getY() > 120) {
                        // up
                        enterNextMonth(gvFlag);
                        return true;
                    } else if (e1.getY() - e2.getY() < -120) {
                        // down
                        enterPrevMonth(gvFlag);
                        return true;
                    }
                    break;
            }
            return false;
        }
    }

    private void enterNextMonth(int gvFlag) {
        addGridView();
        jumpMonth++;
        currentPosition = calV.getCurrentDay();
        calV = new CalendarAdapter(getContext(), jumpMonth, jumpYear, c_year, c_month, c_day, adapterCallback);
        calV.setItemHeight(itemHeight);
        calV.setMondayFirstDay(isMondayFirstDay);
        if (currentPosition != -1) {
            calV.setCurrentSelected(currentPosition);
        }
        calV.setShowLunar(showLunar);
        gridView.setAdapter(calV);
        if (callback != null) {
            callback.onMonthChanged(this, Integer.parseInt(calV.getShowYear()), Integer.parseInt(calV.getShowMonth()));
        }
        gvFlag++;
        flipper.addView(gridView, gvFlag);
        switch (gestureMethod) {
            case GESTURE_HORZ:
                flipper.setInAnimation(AnimationUtils.loadAnimation(getContext(), R.anim.push_left_in));
                flipper.setOutAnimation(AnimationUtils.loadAnimation(getContext(), R.anim.push_left_out));
                break;
            case GESTURE_VERT:
                flipper.setInAnimation(AnimationUtils.loadAnimation(getContext(), R.anim.push_bottom_in));
                flipper.setOutAnimation(AnimationUtils.loadAnimation(getContext(), R.anim.push_bottom_out));
                break;
        }
        flipper.showNext();
        flipper.removeViewAt(0);
    }

    private void enterPrevMonth(int gvFlag) {
        addGridView();
        jumpMonth--;
        currentPosition = calV.getCurrentDay();
        calV = new CalendarAdapter(getContext(), jumpMonth, jumpYear, c_year, c_month, c_day, adapterCallback);
        calV.setItemHeight(itemHeight);
        calV.setMondayFirstDay(isMondayFirstDay);
        if (currentPosition != -1) {
            calV.setCurrentSelected(currentPosition);
        }
        calV.setShowLunar(showLunar);
        gridView.setAdapter(calV);
        gvFlag++;
        if (callback != null) {
            callback.onMonthChanged(this, Integer.parseInt(calV.getShowYear()), Integer.parseInt(calV.getShowMonth()));
        }
        flipper.addView(gridView, gvFlag);

        switch (gestureMethod) {
            case GESTURE_HORZ:
                flipper.setInAnimation(AnimationUtils.loadAnimation(getContext(), R.anim.push_right_in));
                flipper.setOutAnimation(AnimationUtils.loadAnimation(getContext(), R.anim.push_right_out));
                break;
            case GESTURE_VERT:
                flipper.setInAnimation(AnimationUtils.loadAnimation(getContext(), R.anim.push_top_in));
                flipper.setOutAnimation(AnimationUtils.loadAnimation(getContext(), R.anim.push_top_out));
                break;
        }

        flipper.showPrevious();
        flipper.removeViewAt(0);
    }

    public void refreshT() {

        calV = new CalendarAdapter(getContext(), jumpMonth, jumpYear, c_year, c_month, c_day, adapterCallback);
        calV.setItemHeight(itemHeight);
        calV.setMondayFirstDay(isMondayFirstDay);
        if (currentPosition != -1) {
            calV.setCurrentSelected(currentPosition);
        }
        calV.setShowLunar(showLunar);
        gridView.setAdapter(calV);
    }

    public void nextMonth() {
        enterNextMonth(0);
    }

    public void prevMonth() {
        enterPrevMonth(0);
    }

    public void setItemHeight(int height) {
        this.itemHeight = height;
        calV.setItemHeight(height);
        calV.notifyDataSetChanged();
    }

    public void setMondayFirstDay(boolean b) {
        if (isMondayFirstDay != b) {
            isMondayFirstDay = b;
            calV.setMondayFirstDay(isMondayFirstDay);
            for (int i = 0; i < tvWeek.length; i++) {
                if (isMondayFirstDay) {
                    if (i == 6) {
                        tvWeek[i].setText(weekString[0]);
                    } else {
                        tvWeek[i].setText(weekString[i + 1]);
                    }
                } else {
                    tvWeek[i].setText(weekString[i]);
                }
                tvWeek[i].setTextColor(getResources().getColor((i == (isMondayFirstDay ? 5 : 0) || i == 6) ? android.R.color.holo_blue_light : R.color.black));
            }
        }
    }

    public void setAdapterCallback(CalendarAdapter.CalendarAdapterCallback callback) {
        adapterCallback = callback;
        if (calV != null) {
            calV.setCalendarAdapterCallback(adapterCallback);
            calV.reloadData();
        }
    }

    public int getCurrentPosition() {
        return calV.getCurrentSelected();
    }

    public void setCurrentPosition(int pos) {
        currentPosition = pos;
        calV.setCurrentSelected(currentPosition);
    }

    public int getYear() {
        return Integer.parseInt(calV.getShowYear());
    }

    public int getMonth() {
        return Integer.parseInt(calV.getShowMonth());
    }


    public int getDay() {
        return calV.getCurrentDay();
    }

    public int getToday() {
        return calV.getToday();
    }

    public int getJumpYear() {
        return calV.getJumpYear();
    }

    public int getJumpMonth() {
        return calV.getJumpMonth();
    }

}


