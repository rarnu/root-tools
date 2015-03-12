package com.rarnu.devlib.adapter;

import android.content.Context;
import android.graphics.Color;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AbsListView;
import android.widget.BaseAdapter;
import android.widget.TextView;
import com.rarnu.devlib.R;
import com.rarnu.devlib.component.tools.SpecialCalendar;

import java.text.SimpleDateFormat;
import java.util.Calendar;

public class LightCalendarAdapter extends BaseAdapter {

    private int daysOfMonth = 0;
    private int dayOfWeek = 0;
    private Context context;
    private String[] dayNumber = new String[42];
    private SpecialCalendar sc = null;
    private String currentYear = "";
    private String currentMonth = "";
    private int currentFlag = -1;
    private boolean isMondayFirstDay = false;

    public void setMondayFirstDay(boolean b) {
        if (isMondayFirstDay != b) {
            isMondayFirstDay = b;
            reloadData();
        }
    }

    private int itemHeight = ViewGroup.LayoutParams.MATCH_PARENT;

    public LightCalendarAdapter(Context context, int year_c, int month_c) {

        this.context = context;
        sc = new SpecialCalendar();

        int stepYear = year_c;
        int stepMonth = month_c;
        if (stepMonth > 0) {
            if (stepMonth % 12 == 0) {
                stepYear = year_c + stepMonth / 12 - 1;
                stepMonth = 12;
            } else {
                stepYear = year_c + stepMonth / 12;
                stepMonth = stepMonth % 12;
            }
        } else {
            stepYear = year_c - 1 + stepMonth / 12;
            stepMonth = stepMonth % 12 + 12;
            if (stepMonth % 12 == 0) {

            }
        }

        currentYear = String.valueOf(stepYear);
        currentMonth = String.valueOf(stepMonth);
        getCalendar(Integer.parseInt(currentYear), Integer.parseInt(currentMonth));

    }

    public void setItemHeight(int height) {
        this.itemHeight = height;
    }

    public LightCalendarAdapter(Context context, int year, int month, int day) {
        this.context = context;
        sc = new SpecialCalendar();
        currentYear = String.valueOf(year);
        currentMonth = String.valueOf(month);
        getCalendar(Integer.parseInt(currentYear), Integer.parseInt(currentMonth));
    }

    @Override
    public int getCount() {
        return dayNumber.length;
    }

    @Override
    public Object getItem(int position) {
        return dayNumber[position];
    }

    @Override
    public long getItemId(int position) {
        return position;
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {

        if (convertView == null) {
            convertView = LayoutInflater.from(context).inflate(R.layout.light_calendar_item, null);
        }

        AbsListView.LayoutParams vllp = (AbsListView.LayoutParams) convertView.getLayoutParams();
        if (vllp == null) {
            vllp = new AbsListView.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, itemHeight);
        } else {
            vllp.height = itemHeight;
        }
        convertView.setBackgroundColor(Color.WHITE);
        convertView.setLayoutParams(vllp);

        String d = dayNumber[position];

        TextView tvDay = (TextView) convertView.findViewById(R.id.tvDay);
        tvDay.setText(d);
        tvDay.setTextColor(Color.GRAY);

        if (position < daysOfMonth + dayOfWeek && position >= dayOfWeek) {
            tvDay.setTextColor(Color.BLACK);
            if (isMondayFirstDay) {
                if (position % 7 == 5 || position % 7 == 6) {
                    tvDay.setTextColor(context.getResources().getColor(R.color.yiban_blue));
                }
            } else {
                if (position % 7 == 0 || position % 7 == 6) {
                    tvDay.setTextColor(context.getResources().getColor(R.color.yiban_blue));
                }
            }
        }

        if (currentFlag == position) {
            convertView.setBackgroundColor(context.getResources().getColor(R.color.yiban_blue));
            tvDay.setTextColor(Color.WHITE);
        }

        return convertView;
    }

    public void getCalendar(int year, int month) {
        boolean isLeapyear = sc.isLeapYear(year);
        daysOfMonth = sc.getDaysOfMonth(isLeapyear, month);
        dayOfWeek = sc.getWeekdayOfMonth(year, month, isMondayFirstDay);
        getweek(year, month);
    }

    private void getweek(int year, int month) {
        currentFlag = -1;
        Calendar c = Calendar.getInstance();
        int currentYear = c.get(Calendar.YEAR);
        int currentMonth = c.get(Calendar.MONTH) + 1;
        int currentDay = c.get(Calendar.DAY_OF_MONTH);
        for (int i = 0; i < dayNumber.length; i++) {
            if (i < dayOfWeek) {
                dayNumber[i] = "";
            } else if (i < daysOfMonth + dayOfWeek) {
                dayNumber[i] = i - dayOfWeek + 1 + "";
                if (year == currentYear && month == currentMonth && ((i - dayOfWeek + 1) == currentDay)) {
                    currentFlag = i;
                }
            } else {
                dayNumber[i] = "";
            }
        }
    }

    public void reloadData() {
        getCalendar(Integer.parseInt(currentYear), Integer.parseInt(currentMonth));
        notifyDataSetChanged();
    }
}
