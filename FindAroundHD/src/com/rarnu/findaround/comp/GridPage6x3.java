package com.rarnu.findaround.comp;

import android.content.Context;
import android.util.AttributeSet;
import android.util.Log;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.View.OnLongClickListener;
import android.widget.Button;
import android.widget.RelativeLayout;

import com.rarnu.findaround.R;
import com.rarnu.findaround.common.PageItem;
import com.rarnu.findaround.common.PageUtils;
import com.rarnu.findaround.common.UIUtils;

public class GridPage6x3 extends RelativeLayout implements OnClickListener,
		OnLongClickListener {

	public interface OnDeleteClickListener {
		void onDeleteClick(View v, String tag);
	}

	public interface OnKeywordClickListener {
		void onKeywordClick(View v, String tag);
	}

	private ButtonEx[] btns = null;
	private OnKeywordClickListener listener;
	private OnLongClickListener longListener;
	private OnDeleteClickListener deleteListener;

	public GridPage6x3(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
		init();
	}

	public GridPage6x3(Context context, AttributeSet attrs) {
		super(context, attrs);
		init();
	}

	public GridPage6x3(Context context) {
		super(context);
		init();
	}

	private void init() {
		addView(inflate(getContext(), R.layout.welcome_page_item_6x3, null));

		btns = new ButtonEx[18];
		btns[0] = (ButtonEx) findViewById(R.id.btn1);
		btns[1] = (ButtonEx) findViewById(R.id.btn2);
		btns[2] = (ButtonEx) findViewById(R.id.btn3);
		btns[3] = (ButtonEx) findViewById(R.id.btn4);
		btns[4] = (ButtonEx) findViewById(R.id.btn5);
		btns[5] = (ButtonEx) findViewById(R.id.btn6);
		btns[6] = (ButtonEx) findViewById(R.id.btn7);
		btns[7] = (ButtonEx) findViewById(R.id.btn8);
		btns[8] = (ButtonEx) findViewById(R.id.btn9);
		btns[9] = (ButtonEx) findViewById(R.id.btn10);
		btns[10] = (ButtonEx) findViewById(R.id.btn11);
		btns[11] = (ButtonEx) findViewById(R.id.btn12);
		btns[12] = (ButtonEx) findViewById(R.id.btn13);
		btns[13] = (ButtonEx) findViewById(R.id.btn14);
		btns[14] = (ButtonEx) findViewById(R.id.btn15);
		btns[15] = (ButtonEx) findViewById(R.id.btn16);
		btns[16] = (ButtonEx) findViewById(R.id.btn17);
		btns[17] = (ButtonEx) findViewById(R.id.btn18);

		for (int i = 0; i < btns.length; i++) {
			btns[i].setIcon(R.drawable.ic_launcher);
			btns[i].getButton().setOnClickListener(this);
			btns[i].getButton().setOnLongClickListener(this);
			btns[i].getDeleteButton().setOnClickListener(this);
		}

		resizeButtons();
	}

	private void resizeButtons() {
		// 3wx4h
		int width = UIUtils.getWidth();
		Log.e("landscape", String.format("width %d", width));

		// int height = UIUtils.getHeight() - 96;

		width = width - UIUtils.dipToPx(128);
		width = width / 26;

		for (int i = 0; i < btns.length; i++) {
			RelativeLayout.LayoutParams rlp = (RelativeLayout.LayoutParams) btns[i]
					.getLayoutParams();
			rlp.width = width * 3;
			rlp.height = rlp.width;
			rlp.leftMargin = width; // (i % 6 == 0 ? 0 : width);
			rlp.topMargin = (i < 6 ? 0 : width);
			btns[i].setLayoutParams(rlp);
		}
	}

	public void setButtonClickEvent(OnKeywordClickListener listener) {
		this.listener = listener;
	}

	public void setButtonLongClickEvent(OnLongClickListener longListener) {
		this.longListener = longListener;
	}

	public void setDeleteButtonClickEvent(OnDeleteClickListener deleteListener) {
		this.deleteListener = deleteListener;
	}

	public void setButtonsItem(PageItem[] items) {
		int count = items.length;
		if (count > 18) {
			count = 18;
		}
		for (int i = 0; i < count; i++) {
			btns[i].setText(items[i].name);
			btns[i].setFixed(items[i].fixed);
			btns[i].setIcon(PageUtils.getIcon(items[i].image));
			if (items[i].name.equals("")) {
				btns[i].setVisibility(View.INVISIBLE);
			}
		}
	}

	@Override
	public void onClick(View v) {

		if (v instanceof Button) {
			String tag = (String) v.getTag();
			if (tag != null) {
				String[] tags = tag.split("\\|");
				if (tags[0].equals("click")) {
					if (!getEditStatus()) {
						if (listener != null) {
							listener.onKeywordClick(v, tags[1]);
						}
					}
				} else if (tags[0].equals("delete")) {
					if (getEditStatus()) {
						if (deleteListener != null) {
							deleteListener.onDeleteClick(v, tags[1]);
						}
					}
				}
			}
		}

	}

	@Override
	public boolean onLongClick(View v) {
		if (!getEditStatus()) {
			if (longListener != null) {
				return longListener.onLongClick(v);
			}
		}
		return true;
	}

	public void setEditStatus(boolean edit) {
		for (int i = 0; i < btns.length; i++) {
			btns[i].setMode(edit);
		}
	}

	public boolean getEditStatus() {
		return btns[0].getMode();
	}

}
