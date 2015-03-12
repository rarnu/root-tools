package com.rarnu.devlib.adapter;

import android.content.Context;
import android.content.res.Resources;
import android.graphics.Color;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AbsListView;
import android.widget.BaseAdapter;
import android.widget.TextView;
import com.rarnu.devlib.R;
import com.rarnu.devlib.component.tools.LunarCalendar;
import com.rarnu.devlib.component.tools.SpecialCalendar;

import java.text.SimpleDateFormat;
import java.util.Date;

public class CalendarAdapter extends BaseAdapter {

    public interface CalendarAdapterCallback {
        void onBeforeMonthChanging(int year, int month);
        String appendDayData(int year, int month, int day);
        void getCalendarViewItem(View convertView, int year, int month, int day, String lunar, String[] extra, boolean isInMonth, boolean isSelected, boolean isToday);
    }

    private boolean isLeapyear = false;
    private int daysOfMonth = 0;
    private int dayOfWeek = 0;
    private int lastDaysOfMonth = 0;
    private Context context;
    private String[] dayNumber = new String[42];
    private SpecialCalendar sc = null;
    private LunarCalendar lc = null;

    private String currentYear = "";
    private String currentMonth = "";
    private String currentDay = "";

    private SimpleDateFormat sdf = new SimpleDateFormat("yyyy-M-d");
    private int currentFlag = -1;
    private int[] schDateTagFlag = null;

    private String showYear = "";
    private String showMonth = "";
    private String animalsYear = "";
    private String leapMonth = "";
    private String cyclical = "";

    private String sysDate = "";
    private String sys_year = "";
    private String sys_month = "";
    private String sys_day = "";

    private int currentSelected = -1;
    private boolean isMondayFirstDay = false;
    private CalendarAdapterCallback callback;
    private boolean showLunar = true;
    private int jumpYear = 0;
    private int jumpMonth = 0;

    public void setShowLunar(boolean b) {
        showLunar = b;
        notifyDataSetChanged();
    }

    public void setMondayFirstDay(boolean b) {
        if (isMondayFirstDay != b) {
            isMondayFirstDay = b;
            reloadData();
        }
    }

    public void setCalendarAdapterCallback(CalendarAdapterCallback callback) {
        this.callback = callback;
    }

    private int itemHeight = ViewGroup.LayoutParams.MATCH_PARENT;

    public CalendarAdapter() {
        Date date = new Date();
        sysDate = sdf.format(date);
        sys_year = sysDate.split("-")[0];
        sys_month = sysDate.split("-")[1];
        sys_day = sysDate.split("-")[2];
    }

    public CalendarAdapter(Context context, int jumpMonth, int jumpYear, int year_c, int month_c, int day_c, CalendarAdapterCallback callback) {
        this();
        this.context = context;
        this.callback = callback;
        sc = new SpecialCalendar();
        lc = new LunarCalendar();
        this.jumpMonth = jumpMonth;
        this.jumpYear = jumpYear;

        int stepYear = year_c + jumpYear;
        int stepMonth = month_c + jumpMonth;
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
        currentDay = String.valueOf(day_c);

        getCalendar(Integer.parseInt(currentYear), Integer.parseInt(currentMonth));

    }

    public void setItemHeight(int height) {
        this.itemHeight = height;
    }

    public CalendarAdapter(Context context, int year, int month, int day) {
        this();
        this.context = context;
        sc = new SpecialCalendar();
        lc = new LunarCalendar();
        currentYear = String.valueOf(year);
        currentMonth = String.valueOf(month);
        currentDay = String.valueOf(day);
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

    /**
     * if you want to use your own calendar item layout, override this method and returns your layout id
     * @return
     */
    public int getCalendarItemLayout() {
        return 0;
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {

        if (convertView == null) {
            if (getCalendarItemLayout() == 0) {
                convertView = LayoutInflater.from(context).inflate(R.layout.calendar_item, null);
            } else {
                convertView = LayoutInflater.from(context).inflate(getCalendarItemLayout(), null);
            }
        }

        AbsListView.LayoutParams vllp = (AbsListView.LayoutParams) convertView.getLayoutParams();
        if (vllp == null) {
            vllp = new AbsListView.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, itemHeight);
        } else {
            vllp.height = itemHeight;
        }
        convertView.setBackgroundColor(Color.WHITE);
        convertView.setLayoutParams(vllp);

        String[] dnarr = dayNumber[position].split("\\.");
        String d = dnarr[0];
        String dv = dnarr[1];
        boolean evt = false;
        String[] extraArr = null;
        if (dnarr.length > 2) {
            evt = true;
            extraArr =  new String[dnarr.length - 2];
            for (int i=2; i<dnarr.length; i++) {
                extraArr[i-2] = dnarr[i];
            }
        }

        if (getCalendarItemLayout() == 0) {
            TextView tvDay = (TextView) convertView.findViewById(R.id.tvDay);
            TextView tvLunar = (TextView) convertView.findViewById(R.id.tvLunar);
            View vEvent = convertView.findViewById(R.id.vEvent);
            vEvent.setBackgroundColor(context.getResources().getColor(R.color.yiban_blue));
            tvLunar.setVisibility(showLunar ? View.VISIBLE : View.GONE);

            tvDay.setText(d);
            tvLunar.setText(dv);

            tvDay.setTextColor(Color.GRAY);
            tvLunar.setTextColor(Color.GRAY);
            vEvent.setVisibility(View.INVISIBLE);

            if (position < daysOfMonth + dayOfWeek && position >= dayOfWeek) {
                tvDay.setTextColor(Color.BLACK);
                if (isMondayFirstDay) {
                    if (position % 7 == 5 || position % 7 == 6) {
                        tvDay.setTextColor(context.getResources().getColor(R.color.yiban_blue));
                        tvLunar.setTextColor(context.getResources().getColor(R.color.yiban_blue));
                    }
                } else {
                    if (position % 7 == 0 || position % 7 == 6) {
                        tvDay.setTextColor(context.getResources().getColor(R.color.yiban_blue));
                        tvLunar.setTextColor(context.getResources().getColor(R.color.yiban_blue));
                    }
                }
                vEvent.setVisibility(evt ? View.VISIBLE : View.INVISIBLE);
            }

            if (currentSelected == position) {
                convertView.setBackgroundColor(context.getResources().getColor(R.color.lightgray));
                tvDay.setTextColor(Color.WHITE);
                tvLunar.setTextColor(Color.WHITE);
            }

            if (currentFlag == position) {
                convertView.setBackgroundColor(context.getResources().getColor(R.color.yiban_blue));
                tvDay.setTextColor(Color.WHITE);
                tvLunar.setTextColor(Color.WHITE);
                vEvent.setBackgroundColor(Color.WHITE);
            }
        } else {
            if (callback != null) {
                callback.getCalendarViewItem(convertView,
                        Integer.parseInt(currentYear),
                        Integer.parseInt(currentMonth),
                        Integer.parseInt(d), dv, extraArr,
                        (position < daysOfMonth + dayOfWeek && position >= dayOfWeek),
                        (currentSelected == position),
                        (currentFlag == position));
            }
        }

        return convertView;
    }

    public void getCalendar(int year, int month) {
        isLeapyear = sc.isLeapYear(year);
        daysOfMonth = sc.getDaysOfMonth(isLeapyear, month);
        dayOfWeek = sc.getWeekdayOfMonth(year, month, isMondayFirstDay);
        lastDaysOfMonth = sc.getDaysOfMonth(isLeapyear, month - 1);
        getweek(year, month);
    }

    private void getweek(int year, int month) {
        int j = 1;
        int flag = 0;
        String lunarDay = "";
        if (callback != null) {
            callback.onBeforeMonthChanging(year, month);
        }
        setShowYear(String.valueOf(year));
        setShowMonth(String.valueOf(month));
        setAnimalsYear(lc.animalsYear(year));
        setLeapMonth(lc.leapMonth == 0 ? "" : String.valueOf(lc.leapMonth));
        setCyclical(lc.cyclical(year));

        for (int i = 0; i < dayNumber.length; i++) {
            if (i < dayOfWeek) {
                int temp = lastDaysOfMonth - dayOfWeek + 1;
                lunarDay = lc.getLunarDate(year, month - 1, temp + i, false);
                dayNumber[i] = (temp + i) + "." + lunarDay;

            } else if (i < daysOfMonth + dayOfWeek) {
                String day = String.valueOf(i - dayOfWeek + 1);
                lunarDay = lc.getLunarDate(year, month, i - dayOfWeek + 1, false);
                dayNumber[i] = i - dayOfWeek + 1 + "." + lunarDay + ".";
                if (callback != null) {
                    dayNumber[i] += callback.appendDayData(year, month, i - dayOfWeek + 1);
                }

                if (sys_year.equals(String.valueOf(year)) && sys_month.equals(String.valueOf(month)) && sys_day.equals(day)) {
                    currentFlag = i;
                    currentSelected = currentFlag;
                }

            } else {
                lunarDay = lc.getLunarDate(year, month + 1, j, false);
                dayNumber[i] = j + "." + lunarDay;
                j++;
            }
        }

        String abc = "";
        for (int i = 0; i < dayNumber.length; i++) {
            abc = abc + dayNumber[i] + ":";
        }

    }

    public void matchScheduleDate(int year, int month, int day) {

    }

    public String getDateByClickItem(int position) {
        return dayNumber[position];
    }

    public int getStartPositon() {
        return dayOfWeek + 7;
    }

    public int getEndPosition() {
        return (dayOfWeek + daysOfMonth + 7) - 1;
    }

    public String getShowYear() {
        return showYear;
    }

    public void setShowYear(String showYear) {
        this.showYear = showYear;
    }

    public String getShowMonth() {
        return showMonth;
    }

    public void setShowMonth(String showMonth) {
        this.showMonth = showMonth;
    }

    public String getAnimalsYear() {
        return animalsYear;
    }

    public void setAnimalsYear(String animalsYear) {
        this.animalsYear = animalsYear;
    }

    public String getLeapMonth() {
        return leapMonth;
    }

    public void setLeapMonth(String leapMonth) {
        this.leapMonth = leapMonth;
    }

    public String getCyclical() {
        return cyclical;
    }

    public void setCyclical(String cyclical) {
        this.cyclical = cyclical;
    }

    public void setCurrentSelected(int sel) {
        currentSelected = sel;
        notifyDataSetChanged();
    }

    public int getCurrentSelected() {
        return currentSelected;
    }

    public int getCurrentDay() {
        int ret = 0;
        if (currentSelected != -1) {
            ret = Integer.parseInt(dayNumber[currentSelected].split("\\.")[0]);
        }
        return ret;

    }

    public void reloadData() {
        getCalendar(Integer.parseInt(currentYear), Integer.parseInt(currentMonth));
        notifyDataSetChanged();
    }

    public int getJumpYear() {
        return jumpYear;
    }

    public int getJumpMonth() {
        return  jumpMonth;
    }

    public int getToday() {
        return Integer.parseInt(dayNumber[currentFlag].split("\\.")[0]);
    }

    @Override
    public boolean isEnabled(int position) {
        return (position < daysOfMonth + dayOfWeek && position >= dayOfWeek);
    }
}
