package com.rarnu.vim.emotion.comp;

import android.view.View;

public interface OnCalendarTouch {

	void onTouchDown(View v);
	void onTouchUp(View v);
	void onShrink();
	void onExpand();
}
