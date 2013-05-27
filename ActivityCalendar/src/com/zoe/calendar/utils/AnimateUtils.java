package com.zoe.calendar.utils;

import android.view.animation.Animation;
import android.view.animation.RotateAnimation;

public class AnimateUtils {

	public static RotateAnimation getRotateAnimation() {
		RotateAnimation ra = new RotateAnimation(0.0f, 360.0f,
				Animation.RELATIVE_TO_SELF, 0.5f, Animation.RELATIVE_TO_SELF,
				0.5f);
		ra.setDuration(1000);
		ra.setStartOffset(0);
		ra.setRepeatCount(Animation.INFINITE);
		return ra;
	}
}
