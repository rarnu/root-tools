package com.rarnu.utils;

import android.view.animation.Animation;
import android.view.animation.RotateAnimation;
import android.view.animation.ScaleAnimation;

public class AnimateUtils {

    public static RotateAnimation getRotateAnimation(int repeat, int duration) {
        RotateAnimation ra = new RotateAnimation(0.0f, 360.0f, Animation.RELATIVE_TO_SELF, 0.5f, Animation.RELATIVE_TO_SELF, 0.5f);
        ra.setDuration(duration);
        ra.setStartOffset(0);
        ra.setRepeatCount(repeat);
        return ra;
    }

    public static ScaleAnimation getScaleAnimation(float from, float to, int duration, boolean fillAfter) {
        ScaleAnimation sa = new ScaleAnimation(from, to, from, to, Animation.RELATIVE_TO_SELF, 0.5F, Animation.RELATIVE_TO_SELF, 0.5F);

        sa.setDuration(duration);
        sa.setFillBefore(fillAfter);
        sa.setFillEnabled(fillAfter);
        sa.setFillAfter(fillAfter);
        return sa;
    }

}
