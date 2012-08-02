package com.rarnu.findaround.comp;

import android.app.Dialog;
import android.content.Context;
import android.view.Window;
import android.view.WindowManager;

import com.rarnu.findaround.R;
import com.rarnu.findaround.common.UIUtils;

public class PopupMenuDialog extends Dialog {

	public PopupMenuDialog(Context context, int theme) {
		super(context, theme);
		requestWindowFeature(Window.FEATURE_NO_TITLE);
		setContentView(R.layout.popup_menu);

		WindowManager.LayoutParams lp = getWindow().getAttributes();
		int x = (UIUtils.getWidth() - UIUtils.dipToPx(150)) / 2;
		int y = (UIUtils.getHeight() - UIUtils.getStatusbarHeight(context) - UIUtils
				.dipToPx(200)) / 2;
		lp.x = (-x) + UIUtils.dipToPx(4);
		lp.y = (-y) + UIUtils.dipToPx(48);
		getWindow().setAttributes(lp);
	}
}
