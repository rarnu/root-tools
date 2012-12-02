package com.rarnu.zoe.love2;

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;

import android.app.TimePickerDialog;
import android.app.TimePickerDialog.OnTimeSetListener;
import android.util.Log;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.ImageView;
import android.widget.TextView;
import android.widget.TimePicker;

import com.rarnu.zoe.love2.base.BaseActivity;
import com.rarnu.zoe.love2.common.Config;
import com.rarnu.zoe.love2.comp.Title;
import com.rarnu.zoe.love2.utils.AlarmUtils;
import com.rarnu.zoe.love2.utils.MiscUtils;

public class SettingsActivity extends BaseActivity implements OnClickListener {

	TextView tvSet1, tvSetDesc1;
	ImageView chkHint;

	@Override
	protected void setContentView() {
		setContentView(R.layout.activity_settings);

	}

	@Override
	protected void initComponents() {
		super.initComponents();
		title.getBarItem(Title.BARITEM_CENTER)
				.setText(R.string.system_settings);
		title.getBarItem(Title.BARITEM_LEFT).setIcon(R.drawable.home);

		tvSet1 = (TextView) findViewById(R.id.tvSet1);
		tvSetDesc1 = (TextView) findViewById(R.id.tvSetDesc1);
		chkHint = (ImageView) findViewById(R.id.chkHint);

		chkHint.setImageResource(Config.getHintEnabled(this, 1) ? R.drawable.check_yes
				: R.drawable.check_no);

		loadTime(tvSetDesc1, 1, 11);

	}

	private void loadTime(TextView tv, int index, int def) {
		Calendar cDef = Calendar.getInstance();
		cDef.set(Calendar.HOUR_OF_DAY, def);
		cDef.set(Calendar.MINUTE, 0);
		long c = Config.getHintTime(this, index, cDef.getTimeInMillis());
		String timeStr = new SimpleDateFormat("HH:mm").format(new Date(c));
		tv.setText(String.format(getString(R.string.set_desc_hint), timeStr));
	}

	@Override
	protected void initEvents() {
		super.initEvents();
		title.getBarItem(Title.BARITEM_LEFT).setOnButtonClick(this);
		tvSet1.setOnClickListener(this);
		tvSetDesc1.setOnClickListener(this);
		chkHint.setOnClickListener(this);
	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case Title.ITEM_LEFT:
			finish();
			break;
		case R.id.chkHint:
			boolean checked = Config.getHintEnabled(this, 1);
			checked = !checked;
			Config.setHintEnabled(this, 1, checked);
			if (checked) {
				Calendar cDef = MiscUtils.loadDefaultCalendar(6);
				long timeMillis = Config.getHintTime(this, 1,
						cDef.getTimeInMillis());
				Calendar c = MiscUtils.loadTimeMillis(timeMillis);
				AlarmUtils.startAlarm(this, 1, c.get(Calendar.HOUR_OF_DAY),
						c.get(Calendar.MINUTE));
				chkHint.setImageResource(R.drawable.check_yes);
			} else {
				AlarmUtils.cancelAlarm(this, 1);
				chkHint.setImageResource(R.drawable.check_no);
			}
			break;
		case R.id.tvSet1:
		case R.id.tvSetDesc1:
			selectTime();
			break;
		}

	}

	private void selectTime() {
		Calendar cDef = Calendar.getInstance();
		cDef.set(Calendar.HOUR_OF_DAY, 11);
		cDef.set(Calendar.MINUTE, 0);

		long timeMillis = Config.getHintTime(this, 1, cDef.getTimeInMillis());
		Calendar cNew = Calendar.getInstance();
		cNew.setTimeInMillis(timeMillis);

		TimePickerDialog dialog = new TimePickerDialog(this,
				new OnTimeSetListener() {

					@Override
					public void onTimeSet(TimePicker view, int hourOfDay,
							int minute) {
						Log.e("time", String.format("hour:%d, minute:%d",
								hourOfDay, minute));
						Calendar cFinal = Calendar.getInstance();
						cFinal.set(Calendar.HOUR_OF_DAY, hourOfDay);
						cFinal.set(Calendar.MINUTE, minute);
						Config.setHintTime(SettingsActivity.this, 1,
								cFinal.getTimeInMillis());
						String timeStr = new SimpleDateFormat("HH:mm")
								.format(cFinal.getTime());
						tvSetDesc1.setText(String.format(
								getString(R.string.set_desc_hint), timeStr));
						boolean checked = Config.getHintEnabled(
								SettingsActivity.this, 1);
						if (checked) {
							AlarmUtils.startAlarm(SettingsActivity.this, 1,
									hourOfDay, minute);
						}
					}
				}, cNew.get(Calendar.HOUR_OF_DAY), cNew.get(Calendar.MINUTE),
				true);
		dialog.setCanceledOnTouchOutside(false);
		dialog.show();
	}
}
