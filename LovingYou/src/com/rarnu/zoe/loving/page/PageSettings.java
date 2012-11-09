package com.rarnu.zoe.loving.page;

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;

import android.app.AlarmManager;
import android.app.PendingIntent;
import android.app.TimePickerDialog;
import android.app.TimePickerDialog.OnTimeSetListener;
import android.content.Context;
import android.content.Intent;
import android.util.AttributeSet;
import android.util.Log;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.TimePicker;

import com.rarnu.zoe.loving.Config;
import com.rarnu.zoe.loving.R;
import com.rarnu.zoe.loving.base.BasePage;
import com.rarnu.zoe.loving.common.Consts;
import com.rarnu.zoe.loving.utils.MiscUtils;

public class PageSettings extends BasePage implements OnClickListener {

	CheckBox chkHint1, chkHint2, chkHint3;
	Button btnHint1, btnHint2, btnHint3;

	AlarmManager am;

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
		am = (AlarmManager) getContext()
				.getSystemService(Context.ALARM_SERVICE);

		chkHint1 = (CheckBox) findViewById(R.id.chkHint1);
		chkHint2 = (CheckBox) findViewById(R.id.chkHint2);
		chkHint3 = (CheckBox) findViewById(R.id.chkHint3);

		btnHint1 = (Button) findViewById(R.id.btnHint1);
		btnHint2 = (Button) findViewById(R.id.btnHint2);
		btnHint3 = (Button) findViewById(R.id.btnHint3);

		initEvent();
	}

	private void initEvent() {
		chkHint1.setOnClickListener(this);
		chkHint2.setOnClickListener(this);
		chkHint3.setOnClickListener(this);
		btnHint1.setOnClickListener(this);
		btnHint2.setOnClickListener(this);
		btnHint3.setOnClickListener(this);
	}

	@Override
	public void load(String... param) {
		chkHint1.setChecked(Config.getHintEnabled(getContext(), 1));
		chkHint2.setChecked(Config.getHintEnabled(getContext(), 2));
		chkHint3.setChecked(Config.getHintEnabled(getContext(), 3));

		loadTime(btnHint1, 1, 6);
		loadTime(btnHint2, 2, 12);
		loadTime(btnHint3, 3, 18);
	}

	private void loadTime(Button btn, int index, int def) {
		Calendar cDef = Calendar.getInstance();
		cDef.set(Calendar.HOUR_OF_DAY, def);
		cDef.set(Calendar.MINUTE, 0);
		long c = Config
				.getHintTime(getContext(), index, cDef.getTimeInMillis());
		String timeStr = new SimpleDateFormat("HH:mm").format(new Date(c));
		btn.setText(timeStr);
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
				Calendar cDef = Calendar.getInstance();
				cDef.set(Calendar.HOUR_OF_DAY, 6);
				cDef.set(Calendar.MINUTE, 0);
				long timeMillis = Config.getHintTime(getContext(), 1,
						cDef.getTimeInMillis());
				Calendar c = MiscUtils.loadTimeMillis(timeMillis);
				startAlarm(1, c.get(Calendar.HOUR_OF_DAY),
						c.get(Calendar.MINUTE));
			} else {
				cancelAlarm(1);
			}
			break;
		case R.id.chkHint2:
			Config.setHintEnabled(getContext(), 2, chkHint2.isChecked());
			if (chkHint2.isChecked()) {
				Calendar cDef = Calendar.getInstance();
				cDef.set(Calendar.HOUR_OF_DAY, 12);
				cDef.set(Calendar.MINUTE, 0);
				long timeMillis = Config.getHintTime(getContext(), 2,
						cDef.getTimeInMillis());
				Calendar c = MiscUtils.loadTimeMillis(timeMillis);
				startAlarm(2, c.get(Calendar.HOUR_OF_DAY),
						c.get(Calendar.MINUTE));
			} else {
				cancelAlarm(2);
			}
			break;
		case R.id.chkHint3:
			Config.setHintEnabled(getContext(), 3, chkHint3.isChecked());
			if (chkHint3.isChecked()) {
				Calendar cDef = Calendar.getInstance();
				cDef.set(Calendar.HOUR_OF_DAY, 18);
				cDef.set(Calendar.MINUTE, 0);
				long timeMillis = Config.getHintTime(getContext(), 3,
						cDef.getTimeInMillis());
				Calendar c = MiscUtils.loadTimeMillis(timeMillis);
				startAlarm(3, c.get(Calendar.HOUR_OF_DAY),
						c.get(Calendar.MINUTE));
			} else {
				cancelAlarm(2);
			}
			break;
		case R.id.btnHint1:
			selectTime(1);
			break;
		case R.id.btnHint2:
			selectTime(2);
			break;
		case R.id.btnHint3:
			selectTime(3);
			break;
		}

	}

	private void selectTime(final int index) {
		Calendar cDef = Calendar.getInstance();
		switch (index) {
		case 1:
			cDef.set(Calendar.HOUR_OF_DAY, 6);
			break;
		case 2:
			cDef.set(Calendar.HOUR_OF_DAY, 12);
			break;
		case 3:
			cDef.set(Calendar.HOUR_OF_DAY, 18);
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
							btnHint1.setText(timeStr);
							if (chkHint1.isChecked()) {
								startAlarm(1, hourOfDay, minute);
							}
							break;
						case 2:
							btnHint2.setText(timeStr);
							if (chkHint2.isChecked()) {
								startAlarm(2, hourOfDay, minute);
							}
							break;
						case 3:
							btnHint3.setText(timeStr);
							if (chkHint3.isChecked()) {
								startAlarm(3, hourOfDay, minute);
							}
							break;
						}
					}
				}, cNew.get(Calendar.HOUR_OF_DAY), cNew.get(Calendar.MINUTE),
				true);
		dialog.setCanceledOnTouchOutside(false);
		dialog.show();
	}

	private void startAlarm(int index, int hour, int minute) {
		Log.e("startAlarm", String.format("hour:%d, minute:%d", hour, minute));
		Calendar calendar = Calendar.getInstance();
		calendar.setTimeInMillis(System.currentTimeMillis());
		calendar.set(Calendar.HOUR_OF_DAY, hour);
		calendar.set(Calendar.MINUTE, minute);
		calendar.set(Calendar.SECOND, 0);
		calendar.set(Calendar.MILLISECOND, 0);

		if (calendar.getTimeInMillis() < System.currentTimeMillis()) {
			calendar.setTimeInMillis(System.currentTimeMillis()
					+ (24 * 60 * 60 * 1000));
			calendar.set(Calendar.HOUR_OF_DAY, hour);
			calendar.set(Calendar.MINUTE, minute);
			calendar.set(Calendar.SECOND, 0);
			calendar.set(Calendar.MILLISECOND, 0);
		}

		Intent intent = new Intent(Consts.ALARM_ACTION);
		intent.putExtra("index", index);
		PendingIntent pendingIntent = PendingIntent.getBroadcast(getContext(),
				index, intent, 0);
		am.set(AlarmManager.RTC_WAKEUP, calendar.getTimeInMillis(),
				pendingIntent);
	}

	private void cancelAlarm(int index) {
		Intent intent = new Intent(Consts.ALARM_ACTION);
		intent.putExtra("index", index);
		PendingIntent sender = PendingIntent.getBroadcast(getContext(), index,
				intent, 0);
		am.cancel(sender);
	}

}
