package com.rarnu.findaround.comp;

import android.app.Dialog;
import android.content.Context;
import android.view.MotionEvent;
import android.view.View;
import android.view.View.OnTouchListener;
import android.view.Window;
import android.view.WindowManager;
import android.widget.ImageView;
import android.widget.RelativeLayout;

import com.rarnu.findaround.R;
import com.rarnu.findaround.common.UIUtils;

public class PopupMenuDialog extends Dialog implements OnTouchListener  {

	RelativeLayout[] btn;
	ImageView[] img;

	public PopupMenuDialog(Context context, int theme) {
		super(context, theme);
		requestWindowFeature(Window.FEATURE_NO_TITLE);
		setContentView(R.layout.popup_menu);

		btn = new RelativeLayout[2];
		img = new ImageView[2];
		btn[0] = (RelativeLayout) findViewById(R.id.diagBtn1);
		btn[1] = (RelativeLayout) findViewById(R.id.diagBtn2);
		img[0] = (ImageView) findViewById(R.id.imgDiagBtn1);
		img[1] = (ImageView) findViewById(R.id.imgDiagBtn2);

		WindowManager.LayoutParams lp = getWindow().getAttributes();
		int x = (UIUtils.getWidth() - UIUtils.dipToPx(168)) / 2;
		int y = ((UIUtils.getHeight() - UIUtils.getStatusbarHeight(context) - UIUtils
				.dipToPx(145)) / 2);
		lp.x = (-x) + UIUtils.dipToPx(4);
		lp.y = (-y) + UIUtils.dipToPx(32);
		getWindow().setAttributes(lp);
		
		btn[0].setOnTouchListener(this);
		btn[1].setOnTouchListener(this);
		
	}

	public RelativeLayout getButton(int index) {
		return btn[index];
	}

	@Override
	public boolean onTouch(View v, MotionEvent event) {
		switch (event.getAction()) {
		case MotionEvent.ACTION_DOWN:
			switch (v.getId()) {
			case R.id.diagBtn1:
				img[0].setBackgroundResource(R.drawable.add_focus);
				break;
			case R.id.diagBtn2:
				img[1].setBackgroundResource(R.drawable.settings_focus);
				break;
			}
			break;
		case MotionEvent.ACTION_UP:
			switch (v.getId()) {
			case R.id.diagBtn1:
				img[0].setBackgroundResource(R.drawable.add);
				break;
			case R.id.diagBtn2:
				img[1].setBackgroundResource(R.drawable.settings);
				break;
			}
			break;
		}
		return false;
	}

}
