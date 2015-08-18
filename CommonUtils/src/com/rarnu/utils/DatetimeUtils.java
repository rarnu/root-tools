package com.rarnu.utils;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;

/**
 * Created by rarnu on 3/4/15.
 */
public class DatetimeUtils {

    public static Calendar calendarFromTimeInMillis(long millis) {
        Calendar c = Calendar.getInstance();
        c.setTimeInMillis(millis);
        return c;
    }

    public static long calendarToTimeInMillis(Calendar c) {
        return c.getTimeInMillis();
    }

    public static Calendar offsetYears(Calendar c, int year) {
        c.add(Calendar.YEAR, year);
        return c;
    }

    public static Calendar offsetMonths(Calendar c, int month) {
        c.add(Calendar.MONTH, month);
        return c;
    }

    public static Calendar offsetDays(Calendar c, int day) {
        c.add(Calendar.DAY_OF_YEAR, day);
        return c;
    }

    public static Calendar offsetHours(Calendar c, int hour) {
        c.add(Calendar.HOUR_OF_DAY, hour);
        return c;
    }

    public static Calendar offsetMinutes(Calendar c, int minute) {
        c.add(Calendar.MINUTE, minute);
        return c;
    }

    public static Calendar offsetSeconds(Calendar c, int second) {
        c.add(Calendar.SECOND, second);
        return c;
    }

    public static List<Integer> getSameWeekdaysInMonth(int year, int month, int day, int currentYear, int currentMonth) {
        // get same weekdays in month
        Calendar cThen = Calendar.getInstance();
        cThen.set(Calendar.YEAR, year);
        cThen.set(Calendar.MONTH, month - 1);
        cThen.set(Calendar.DAY_OF_MONTH, day);
        int weekThen = cThen.get(Calendar.DAY_OF_WEEK);
        Calendar cNow = Calendar.getInstance();
        cNow.set(Calendar.YEAR, currentYear);
        cNow.set(Calendar.MONTH, currentMonth - 1);
        cNow.set(Calendar.DAY_OF_MONTH, 1);

        List<Integer> listNow = new ArrayList<Integer>();
        int weekNow = -1;
        for (int i = 0; i < 7; i++) {
            weekNow = cNow.get(Calendar.DAY_OF_WEEK);
            if (weekNow == weekThen) {
                break;
            }
            cNow.add(Calendar.DAY_OF_MONTH, 1);
        }
        int currentDay = cNow.get(Calendar.DAY_OF_MONTH);
        while (currentDay <= 31) {
            listNow.add(currentDay);
            currentDay += 7;
        }

        return listNow;
    }

    public static boolean compareWeekday(int year, int month, int day, int currentYear, int currentMonth, int currentDay) {
        // compare 2 date has same week number
        Calendar cThen = Calendar.getInstance();
        cThen.set(Calendar.YEAR, year);
        cThen.set(Calendar.MONTH, month - 1);
        cThen.set(Calendar.DAY_OF_MONTH, day);
        Calendar cNow = Calendar.getInstance();
        cNow.set(Calendar.YEAR, currentYear);
        cNow.set(Calendar.MONTH, currentMonth - 1);
        cNow.set(Calendar.DAY_OF_MONTH, currentDay);
        int weekThen = cThen.get(Calendar.DAY_OF_WEEK);
        int weekNow = cNow.get(Calendar.DAY_OF_WEEK);
        return weekThen == weekNow;
    }

    public static Calendar getNextWeekday(int year, int month, int day) {
        Calendar cThen = Calendar.getInstance();
        cThen.set(Calendar.YEAR, year);
        cThen.set(Calendar.MONTH, month - 1);
        cThen.set(Calendar.DAY_OF_MONTH, day);
        int weekThen = cThen.get(Calendar.DAY_OF_WEEK);
        Calendar cNow = Calendar.getInstance();
        cNow.add(Calendar.DAY_OF_MONTH, 1); // cannot be today
        int weekNow = -1;
        for (int i=0; i<7; i++) {
            weekNow = cNow.get(Calendar.DAY_OF_WEEK);
            if (weekNow == weekThen) {
                break;
            }
            cNow.add(Calendar.DAY_OF_MONTH, 1);
        }
        return cNow;
    }

    public static long millisBetween(Calendar fromDate, Calendar toDate) {
        return toDate.getTimeInMillis() - fromDate.getTimeInMillis();
    }

    public static boolean isDateAfter(Calendar now, Calendar base) {
        long iNow = now.getTimeInMillis();
        long iBase = base.getTimeInMillis();
        return iNow > iBase;
    }
}
