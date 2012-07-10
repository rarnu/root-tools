package com.rarnu.tools.root.comp;

import android.app.Dialog;
import android.content.Context;
import android.view.View;
import android.view.Window;
import android.widget.Button;
import android.widget.TextView;

import com.rarnu.tools.root.R;

public class AlertDialogEx extends Dialog implements
		android.view.View.OnClickListener {

	// [region] field define
	Button btnOK, btnCancel;
	TextView tvTitle, tvMessage;
	DialogButtonClickListener listenerPositive = null,
			listenerNegative = null;
	// [/region]

	// [region] constructor
	public AlertDialogEx(Context context) {
		super(context);
		getWindow().requestFeature(Window.FEATURE_NO_TITLE);

		setContentView(R.layout.alert_dialog);
		btnOK = (Button) findViewById(R.id.btnOK);
		btnCancel = (Button) findViewById(R.id.btnCancel);
		tvTitle = (TextView) findViewById(R.id.tvTitle);
		tvMessage = (TextView) findViewById(R.id.tvMessage);
		btnOK.setOnClickListener(this);
		btnCancel.setOnClickListener(this);
		setCanceledOnTouchOutside(false);

	}
	// [/region]
	
	// [region] common
	public void setTitle(CharSequence text) {
		tvTitle.setText(text);
	}

	public void setMessage(CharSequence text) {
		tvMessage.setText(text);
	}

	public void setPositiveText(CharSequence text) {
		btnOK.setText(text);
		if (text == null || text.toString().equals("")) {
			btnOK.setVisibility(View.GONE);
		} else {
			btnCancel.setVisibility(View.VISIBLE);
		}
	}

	public void setNegativeText(CharSequence text) {
		btnCancel.setText(text);
		if (text == null || text.toString().equals("")) {
			btnCancel.setVisibility(View.GONE);
		} else {
			btnCancel.setVisibility(View.VISIBLE);
		}
	}

	public void setPositiveClick(DialogButtonClickListener listener) {
		listenerPositive = listener;
	}

	public void setNegativeClick(DialogButtonClickListener listener) {
		listenerNegative = listener;
	}

	// [/region]
	
	// [region] interface define
	public interface DialogButtonClickListener {
		void onClick(View v);
	}

	// [/region]
	
	// [region] events
	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnOK:
			if (listenerPositive != null) {
				listenerPositive.onClick(btnOK);
			}
			dismiss();
			break;
		case R.id.btnCancel:
			if (listenerNegative != null) {
				listenerNegative.onClick(btnCancel);
			}
			dismiss();
			break;
		}

	}
	// [/region]
	
	// [region] public call
	public static void showAlertDialogEx(Context context, String title, String message, String positiveText, DialogButtonClickListener positiveListener,
			String negativeText, DialogButtonClickListener negativeListener) {
		AlertDialogEx dialog = new AlertDialogEx(context);
		dialog.setTitle(title);
		dialog.setMessage(message);
		dialog.setPositiveText(positiveText);
		dialog.setNegativeText(negativeText);
		dialog.setPositiveClick(positiveListener);
		dialog.setNegativeClick(negativeListener);
		dialog.show();
	}

	// [/region]
}
