package com.rarnu.utils;

import java.util.Comparator;

public class CompareUtils<T> {

    public static final int CU_EQUAL = 0;
    public static final int CU_LEFT_BIGGER_THAN_RIGHT = 1;
    public static final int CU_LEFT_SMALLER_THAN_RIGHT = -1;

    public interface CompareCallback<T> {
        int onCompare(CompareUtils util, T left, T right);
    }

    private Comparator<T> comp;
    private CompareCallback<T> innerCallback;

    public CompareUtils(CompareCallback<T> callback) {
        this.innerCallback = callback;
        comp = new Comparator<T>() {
            @Override
            public int compare(T lhs, T rhs) {
                int ret = 0;
                if (innerCallback != null) {
                    ret = innerCallback.onCompare(CompareUtils.this, lhs, rhs);
                }
                return ret;
            }
        };
    }

    public Comparator<T> getComparator() {
        return comp;
    }

}
