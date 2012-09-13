package com.rarnu.tools.root.comp;

import android.app.Dialog;
import android.content.Context;
import android.graphics.Color;
import android.view.Gravity;
import android.view.View;
import android.view.Window;
import android.widget.Button;
import android.widget.LinearLayout;
import android.widget.LinearLayout.LayoutParams;
import android.widget.TextView;

import com.rarnu.tools.root.R;
import com.rarnu.tools.root.utils.UIUtils;

public class RadioDialogEx extends Dialog implements
		android.view.View.OnClickListener {

	// [region] field define
	Button btnCancel;
	TextView tvTitle;
	LinearLayout layRadioItems;
	DialogButtonClickListener listenerNegative = null;
	DialogRadioClickListener radioListener = null;

	// [/region]

	// [region] constructor
	public RadioDialogEx(Context context) {
		super(context);
		getWindow().requestFeature(Window.FEATURE_NO_TITLE);

		setContentView(R.layout.radio_dialog);
		btnCancel = (Button) findViewById(R.id.btnCancel);
		tvTitle = (TextView) findViewById(R.id.tvTitle);
		layRadioItems = (LinearLayout) findViewById(R.id.layRadioItems);
		btnCancel.setOnClickListener(this);
		setCanceledOnTouchOutside(false);

	}

	// [/region]

	// [region] common
	public void setTitle(CharSequence text) {
		tvTitle.setText(text);
	}

	public void setNegativeText(CharSequence text) {
		btnCancel.setText(text);
		if (text == null || text.toString().equals("")) {
			btnCancel.setVisibility(View.GONE);
		} else {
			btnCancel.setVisibility(View.VISIBLE);
		}
	}

	public void setNegativeClick(DialogButtonClickListener listener) {
		listenerNegative = listener;
	}

	public void SetRadioClickListener(DialogRadioClickListener listener) {
		radioListener = listener;
	}

	public void setMessage(String[] messages) {
		int idx = 0;
		if (messages != null) {
			for (String msg : messages) {
				if (msg != null && !msg.trim().equals("")) {
					TextView tv = new TextView(getContext());
					tv.setLayoutParams(new LinearLayout.LayoutParams(
							LayoutParams.MATCH_PARENT, UIUtils.dipToPx(48)));
					tv.setBackgroundResource(R.drawable.radio_item_background);
					tv.setClickable(true);
					tv.setTextColor(Color.BLACK);
					tv.setGravity(Gravity.LEFT | Gravity.CENTER_VERTICAL);
					tv.setText(msg);
					tv.setPadding(UIUtils.dipToPx(8), 0, 0, 0);
					tv.setTag(idx);
					idx++;
					tv.setOnClickListener(this);
					layRadioItems.addView(tv);
				}
			}
		}
	}

	// [/region]

	// [region] interface define
	public interface DialogButtonClickListener {
		void onClick(View v);
	}

	public interface DialogRadioClickListener {
		void onRadioClick(View v, int index);
	}

	// [/region]

	// [region] events
	@Override
	public void onClick(View v) {
		if (v.getId() == R.id.btnCancel) {
			if (listenerNegative != null) {
				listenerNegative.onClick(btnCancel);
			}
			dismiss();
			return;
		} else if (v instanceof TextView) {
			int idx = (Integer) v.getTag();
			if (radioListener != null) {
				radioListener.onRadioClick(v, idx);
			}
			dismiss();
		}

	}

	// [/region]

	// [region] public call
	public static void showRadioDialogEx(Context context, String title,
			String[] message, DialogRadioClickListener radioListener,
			String negativeText, DialogButtonClickListener negativeListener) {
		RadioDialogEx dialog = new RadioDialogEx(context);
		dialog.setTitle(title);
		dialog.setMessage(message);
		dialog.SetRadioClickListener(radioListener);
		dialog.setNegativeText(negativeText);
		dialog.setNegativeClick(negativeListener);
		dialog.show();
	}

	// [/region]
}
