package com.rarnu.zoe.loving.page;

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;

import android.app.TimePickerDialog;
import android.app.TimePickerDialog.OnTimeSetListener;
import android.content.Context;
import android.util.AttributeSet;
import android.util.Log;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.CheckBox;
import android.widget.RelativeLayout;
import android.widget.TextView;
import android.widget.TimePicker;

import com.rarnu.zoe.loving.Config;
import com.rarnu.zoe.loving.R;
import com.rarnu.zoe.loving.base.BasePage;
import com.rarnu.zoe.loving.utils.AlarmUtils;
import com.rarnu.zoe.loving.utils.MiscUtils;

public class PageSettings extends BasePage implements OnClickListener {

	CheckBox chkHint1, chkHint2;
	RelativeLayout layHint1, layHint2, layHint3, layHint4;
	TextView tvHintValue1, tvHintValue2;

	public PageSettings(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
	}

	public PageSettings(Context context, AttributeSet attrs) {
		super(context, attrs);
	}

	public PageSettings(Context context) {
		super(context);
	}

	@Override
	protected void requireRootLayoutId() {
		this.rootLayout = R.layout.page_settings;

	}

	@Override
	protected void init() {
		chkHint1 = (CheckBox) findViewById(R.id.chkHint1);
		chkHint2 = (CheckBox) findViewById(R.id.chkHint2);
		layHint1 = (RelativeLayout) findViewById(R.id.layHint1);
		layHint2 = (RelativeLayout) findViewById(R.id.layHint2);
		layHint3 = (RelativeLayout) findViewById(R.id.layHint3);
		layHint4 = (RelativeLayout) findViewById(R.id.layHint4);
		tvHintValue1 = (TextView) findViewById(R.id.tvHintValue1);
		tvHintValue2 = (TextView) findViewById(R.id.tvHintValue2);

		initEvent();
	}

	private void initEvent() {
		chkHint1.setOnClickListener(this);
		chkHint2.setOnClickListener(this);
		layHint1.setOnClickListener(this);
		layHint2.setOnClickListener(this);
		layHint3.setOnClickListener(this);
		layHint4.setOnClickListener(this);
	}

	@Override
	public void load(String... param) {
		chkHint1.setChecked(Config.getHintEnabled(getContext(), 1));
		chkHint2.setChecked(Config.getHintEnabled(getContext(), 2));

		loadTime(tvHintValue1, 1, 11);
		loadTime(tvHintValue2, 2, 20);
	}

	private void loadTime(TextView tv, int index, int def) {
		Calendar cDef = Calendar.getInstance();
		cDef.set(Calendar.HOUR_OF_DAY, def);
		cDef.set(Calendar.MINUTE, 0);
		long c = Config
				.getHintTime(getContext(), index, cDef.getTimeInMillis());
		String timeStr = new SimpleDateFormat("HH:mm").format(new Date(c));
		tv.setText(timeStr);
	}

	@Override
	public void refresh() {

	}

	@Override
	public void delete(int index) {

	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.chkHint1:
			Config.setHintEnabled(getContext(), 1, chkHint1.isChecked());
			if (chkHint1.isChecked()) {
				Calendar cDef = MiscUtils.loadDefaultCalendar(6);
				long timeMillis = Config.getHintTime(getContext(), 1,
						cDef.getTimeInMillis());
				Calendar c = MiscUtils.loadTimeMillis(timeMillis);
				AlarmUtils.startAlarm(getContext(), 1,
						c.get(Calendar.HOUR_OF_DAY), c.get(Calendar.MINUTE));
			} else {
				AlarmUtils.cancelAlarm(getContext(), 1);
			}
			break;
		case R.id.chkHint2:
			Config.setHintEnabled(getContext(), 2, chkHint2.isChecked());
			if (chkHint2.isChecked()) {
				Calendar cDef = MiscUtils.loadDefaultCalendar(12);
				long timeMillis = Config.getHintTime(getContext(), 2,
						cDef.getTimeInMillis());
				Calendar c = MiscUtils.loadTimeMillis(timeMillis);
				AlarmUtils.startAlarm(getContext(), 2,
						c.get(Calendar.HOUR_OF_DAY), c.get(Calendar.MINUTE));
			} else {
				AlarmUtils.cancelAlarm(getContext(), 2);
			}
			break;

		case R.id.layHint1:
			selectTime(1);
			break;
		case R.id.layHint2:
			selectTime(2);
			break;
		case R.id.layHint3:
			// TODO: share
			break;
		case R.id.layHint4:
			// TODO: about
			break;
		}

	}

	private void selectTime(final int index) {
		Calendar cDef = Calendar.getInstance();
		switch (index) {
		case 1:
			cDef.set(Calendar.HOUR_OF_DAY, 11);
			break;
		case 2:
			cDef.set(Calendar.HOUR_OF_DAY, 20);
			break;
		}
		cDef.set(Calendar.MINUTE, 0);

		long timeMillis = Config.getHintTime(getContext(), index,
				cDef.getTimeInMillis());
		Calendar cNew = Calendar.getInstance();
		cNew.setTimeInMillis(timeMillis);

		TimePickerDialog dialog = new TimePickerDialog(getContext(),
				new OnTimeSetListener() {

					@Override
					public void onTimeSet(TimePicker view, int hourOfDay,
							int minute) {
						Log.e("time", String.format("hour:%d, minute:%d",
								hourOfDay, minute));
						Calendar cFinal = Calendar.getInstance();
						cFinal.set(Calendar.HOUR_OF_DAY, hourOfDay);
						cFinal.set(Calendar.MINUTE, minute);
						Config.setHintTime(getContext(), index,
								cFinal.getTimeInMillis());
						String timeStr = new SimpleDateFormat("HH:mm")
								.format(cFinal.getTime());
						switch (index) {
						case 1:
							tvHintValue1.setText(timeStr);
							if (chkHint1.isChecked()) {
								AlarmUtils.startAlarm(getContext(), 1,
										hourOfDay, minute);
							}
							break;
						case 2:
							tvHintValue2.setText(timeStr);
							if (chkHint2.isChecked()) {
								AlarmUtils.startAlarm(getContext(), 2,
										hourOfDay, minute);
							}
							break;

						}
					}
				}, cNew.get(Calendar.HOUR_OF_DAY), cNew.get(Calendar.MINUTE),
				true);
		dialog.setCanceledOnTouchOutside(false);
		dialog.show();
	}

}
