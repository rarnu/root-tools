package com.zoe.calendar.fragment;

import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.TextView;

import com.rarnu.devlib.base.BaseDialogFragment;
import com.zoe.calendar.R;

public class EventDialogFragment extends BaseDialogFragment implements
		OnClickListener {

	Button btnOK;
	TextView tvEventText;

	// update
	public EventDialogFragment(String tag) {
		super(tag);
	}

	@Override
	public int getBarTitle() {
		return 0;
	}

	@Override
	public int getBarTitleWithPath() {
		return 0;
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	public void initComponents() {
		btnOK = (Button) innerView.findViewById(R.id.btnOK);
		tvEventText = (TextView) innerView.findViewById(R.id.tvEventText);
	}

	@Override
	public void initEvents() {
		btnOK.setOnClickListener(this);
	}

	@Override
	public void initLogic() {
		boolean deleted = getActivity().getIntent().getBooleanExtra("deleted",
				false);
		tvEventText.setText(deleted ? R.string.event_deleted
				: R.string.event_added);
	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.dialog_event;
	}

	@Override
	public String getMainActivityName() {
		return "";
	}

	@Override
	public void initMenu(Menu menu) {

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
		getActivity().finish();

	}
}
