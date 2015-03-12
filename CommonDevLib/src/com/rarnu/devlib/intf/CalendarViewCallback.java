package com.rarnu.devlib.intf;

import android.view.View;

/**
 * Created by rarnu on 2/3/15.
 */
public interface CalendarViewCallback {
    void onCalendarDoubleTap(View v);
    void onMonthChanged(View v, int year, int month);
    void onDayClicked(View v, int year, int month, int day, String lunar);
}
