package com.zoe.calendar.fragment;

import java.util.Calendar;

import android.app.Activity;
import android.app.DatePickerDialog;
import android.app.DatePickerDialog.OnDateSetListener;
import android.app.TimePickerDialog;
import android.app.TimePickerDialog.OnTimeSetListener;
import android.content.Intent;
import android.graphics.Color;
import android.os.Bundle;
import android.view.Gravity;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.DatePicker;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;
import android.widget.TimePicker;
import android.widget.Toast;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.UIUtils;
import com.zoe.calendar.R;
import com.zoe.calendar.common.MenuIds;
import com.zoe.calendar.dialog.TagDialog;
import com.zoe.calendar.utils.APIUtils;
import com.zoe.calendar.utils.DateUtils;

public class SubmitFragment extends BaseFragment implements OnClickListener {

	MenuItem miSend;

	EditText etTitle, etLocation;
	Button dpStart, tpStart, dpEnd, tpEnd;
	EditText etLink, etWeight, etContent;
	LinearLayout layTags;
	Calendar cStart, cEnd;
	ImageView btnAdd;

	boolean isStartDateSet = false, isStartTimeSet = false,
			isEndDateSet = false, isEndTimeSet = false;

	public SubmitFragment(String tag) {
		super(tag, "");
	}

	@Override
	public int getBarTitle() {
		return R.string.submit_name;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.submit_name;
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	public void initComponents() {
		cStart = Calendar.getInstance();
		cStart.set(Calendar.HOUR_OF_DAY, 0);
		cStart.set(Calendar.MINUTE, 0);
		cStart.set(Calendar.SECOND, 0);
		cStart.set(Calendar.MILLISECOND, 0);

		cEnd = (Calendar) cStart.clone();

		dpStart = (Button) innerView.findViewById(R.id.dpStart);
		tpStart = (Button) innerView.findViewById(R.id.tpStart);
		dpEnd = (Button) innerView.findViewById(R.id.dpEnd);
		tpEnd = (Button) innerView.findViewById(R.id.tpEnd);

		layTags = (LinearLayout) innerView.findViewById(R.id.layTags);
		etTitle = (EditText) innerView.findViewById(R.id.etTitle);
		etLocation = (EditText) innerView.findViewById(R.id.etLocation);
		etLink = (EditText) innerView.findViewById(R.id.etLink);
		etWeight = (EditText) innerView.findViewById(R.id.etWeight);
		etContent = (EditText) innerView.findViewById(R.id.etContent);

		btnAdd = (ImageView) innerView.findViewById(R.id.btnAdd);
	}

	@Override
	public void initEvents() {
		dpStart.setOnClickListener(this);
		tpStart.setOnClickListener(this);
		dpEnd.setOnClickListener(this);
		tpEnd.setOnClickListener(this);
		btnAdd.setOnClickListener(this);
	}

	@Override
	public void initLogic() {
		// startActivity(new Intent(getActivity(), NotImplementedDialog.class));
	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.fragment_submit_activity;
	}

	@Override
	public String getMainActivityName() {
		return "";
	}

	@Override
	public void initMenu(Menu menu) {
		miSend = menu.add(0, MenuIds.MENU_SUBMIT, 21, R.string.submit_name);
		miSend.setIcon(android.R.drawable.ic_menu_send);
		miSend.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case MenuIds.MENU_SUBMIT:
			// submit new activity
			submitNewActivity();
			break;
		}
		return true;
	}

	private void submitNewActivity() {
		// check data
		String title = etTitle.getText().toString();
		String location = etLocation.getText().toString();
		String url = etLink.getText().toString();
		String startDate = DateUtils.formatDate(cStart);
		String startTime = DateUtils.formatTime(cStart);
		String endDate = DateUtils.formatDate(cEnd);
		String endTime = DateUtils.formatTime(cEnd);
		String strWeight = etWeight.getText().toString();
		String content = etContent.getText().toString();
		String source = getString(R.string.submit_source);

		String[] tags = new String[layTags.getChildCount()];
		for (int i = 0; i < layTags.getChildCount(); i++) {
			tags[i] = ((TextView) layTags.getChildAt(i)).getText().toString();
		}

		if (title.equals("")) {
			Toast.makeText(getActivity(), R.string.submit_must_has_title,
					Toast.LENGTH_SHORT).show();
			return;
		}

		if (url.equals("")) {
			Toast.makeText(getActivity(), R.string.submit_must_has_url,
					Toast.LENGTH_SHORT).show();
			return;
		}

		if ((!isStartDateSet) || (!isStartTimeSet) || (!isEndDateSet)
				|| (!isEndTimeSet)) {
			Toast.makeText(getActivity(), R.string.submit_must_has_datetime,
					Toast.LENGTH_SHORT).show();
			return;
		}

		if (tags == null || tags.length == 0) {
			Toast.makeText(getActivity(), R.string.submit_must_has_tags,
					Toast.LENGTH_SHORT).show();
			return;
		}

		if (content.equals("")) {
			Toast.makeText(getActivity(), R.string.submit_must_has_content,
					Toast.LENGTH_SHORT).show();
			return;
		}

		// submit data
		/**
		 * final Context context, final String title, final String location,
		 * final String url, final String startDate, final String startTime,
		 * final String endDate, final String endTime, final int weight, final
		 * String[] tags, final String content, final String source
		 */
		int weight = 0;
		try {
			weight = Integer.parseInt(strWeight);
		} catch (Exception e) {

		}
		APIUtils.submitNewActivity(getActivity(), title, location, url,
				startDate, startTime, endDate, endTime, weight, tags, content,
				source);
		Toast.makeText(getActivity(), R.string.submit_done, Toast.LENGTH_LONG)
				.show();
		getActivity().finish();
	}

	@Override
	public void onGetNewArguments(Bundle bn) {

	}

	@Override
	public Bundle getFragmentState() {
		return null;
	}

	@Override
	public void onClick(View v) {

		switch (v.getId()) {
		case R.id.dpStart:
			new DatePickerDialog(getActivity(), new OnDateSetListener() {
				@Override
				public void onDateSet(DatePicker view, int year,
						int monthOfYear, int dayOfMonth) {
					cStart.set(Calendar.YEAR, year);
					cStart.set(Calendar.MONTH, monthOfYear);
					cStart.set(Calendar.DAY_OF_MONTH, dayOfMonth);
					dpStart.setText(DateUtils.formatDate(cStart));
					isStartDateSet = true;
				}
			}, cStart.get(Calendar.YEAR), cStart.get(Calendar.MONTH),
					cStart.get(Calendar.DAY_OF_MONTH)).show();
			break;
		case R.id.tpStart:
			new TimePickerDialog(
					getActivity(),
					new OnTimeSetListener() {

						@Override
						public void onTimeSet(TimePicker view, int hourOfDay,
								int minute) {
							cStart.set(Calendar.HOUR_OF_DAY, hourOfDay);
							cStart.set(Calendar.MINUTE, minute);
							tpStart.setText(DateUtils.formatTime(cStart));
							isStartTimeSet = true;
						}
					}, cStart.get(Calendar.HOUR_OF_DAY), cStart
							.get(Calendar.MINUTE), true).show();
			break;
		case R.id.dpEnd:
			new DatePickerDialog(getActivity(), new OnDateSetListener() {
				@Override
				public void onDateSet(DatePicker view, int year,
						int monthOfYear, int dayOfMonth) {
					cEnd.set(Calendar.YEAR, year);
					cEnd.set(Calendar.MONTH, monthOfYear);
					cEnd.set(Calendar.DAY_OF_MONTH, dayOfMonth);
					dpEnd.setText(DateUtils.formatDate(cEnd));
					isEndDateSet = true;
				}
			}, cEnd.get(Calendar.YEAR), cEnd.get(Calendar.MONTH),
					cEnd.get(Calendar.DAY_OF_MONTH)).show();
			break;
		case R.id.tpEnd:
			new TimePickerDialog(getActivity(), new OnTimeSetListener() {
				@Override
				public void onTimeSet(TimePicker view, int hourOfDay, int minute) {
					cEnd.set(Calendar.HOUR_OF_DAY, hourOfDay);
					cEnd.set(Calendar.MINUTE, minute);
					tpEnd.setText(DateUtils.formatTime(cEnd));
					isEndTimeSet = true;
				}
			}, cEnd.get(Calendar.HOUR_OF_DAY), cEnd.get(Calendar.MINUTE), true)
					.show();
			break;
		case R.id.btnAdd:
			if (layTags.getChildCount() >= 3) {
				Toast.makeText(getActivity(), R.string.tag_too_much,
						Toast.LENGTH_SHORT).show();
			} else {
				startActivityForResult(new Intent(getActivity(),
						TagDialog.class), 0);
			}
			break;
		}

	}

	@Override
	public void onActivityResult(int requestCode, int resultCode, Intent data) {
		if (resultCode != Activity.RESULT_OK) {
			return;
		}
		switch (requestCode) {
		case 0:
			addTag(data.getStringExtra("tag"));
			break;
		}
	}

	private boolean canAddTag(String tag) {
		boolean ret = true;
		if (layTags.getChildCount() >= 3) {
			ret = false;
		}
		if (ret) {
			for (int i = 0; i < layTags.getChildCount(); i++) {
				if (((TextView) layTags.getChildAt(i)).getText().toString()
						.equals(tag)) {
					ret = false;
					break;
				}
			}
		}
		return ret;
	}

	private void addTag(String tag) {
		if (canAddTag(tag)) {
			TextView tvTag = new TextView(getActivity());
			tvTag.setGravity(Gravity.CENTER);
			tvTag.setLayoutParams(new LinearLayout.LayoutParams(UIUtils
					.dipToPx(60), UIUtils.dipToPx(40)));
			tvTag.setTextColor(Color.WHITE);
			tvTag.setTextSize(14F);
			tvTag.setText(tag);
			tvTag.setBackgroundResource(R.drawable.btn_common_style);
			tvTag.setClickable(true);
			tvTag.setOnClickListener(new OnClickListener() {

				@Override
				public void onClick(View v) {
					layTags.removeView(v);
				}
			});
			layTags.addView(tvTag);
		} else {
			Toast.makeText(getActivity(), R.string.tag_already_added,
					Toast.LENGTH_SHORT).show();
		}
	}

}
