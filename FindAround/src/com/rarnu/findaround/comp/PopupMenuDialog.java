package com.rarnu.findaround.comp;

import android.app.Dialog;
import android.content.Context;
import android.view.Window;
import android.view.WindowManager;
import android.widget.Button;

import com.rarnu.findaround.R;
import com.rarnu.findaround.common.UIUtils;

public class PopupMenuDialog extends Dialog {

	Button[] btn;

	public PopupMenuDialog(Context context, int theme) {
		super(context, theme);
		requestWindowFeature(Window.FEATURE_NO_TITLE);
		setContentView(R.layout.popup_menu);

		btn = new Button[2];
		btn[0] = (Button) findViewById(R.id.diagBtn1);
		btn[1] = (Button) findViewById(R.id.diagBtn2);

		WindowManager.LayoutParams lp = getWindow().getAttributes();
		int x = (UIUtils.getWidth() - UIUtils.dipToPx(150)) / 2;
		int y = (UIUtils.getHeight() - UIUtils.getStatusbarHeight(context) - UIUtils
				.dipToPx(200)) / 2;
		lp.x = (-x) + UIUtils.dipToPx(4);
		lp.y = (-y) + UIUtils.dipToPx(48);
		getWindow().setAttributes(lp);
	}

	public Button getButton(int index) {
		return btn[index];
	}
}
