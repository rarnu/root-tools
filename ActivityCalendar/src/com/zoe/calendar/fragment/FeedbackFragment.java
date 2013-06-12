package com.zoe.calendar.fragment;

import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.EditText;
import android.widget.Toast;

import com.rarnu.devlib.base.BaseFragment;
import com.zoe.calendar.R;
import com.zoe.calendar.common.MenuIds;
import com.zoe.calendar.utils.APIUtils;
import com.zoe.calendar.utils.ResourceUtils;

public class FeedbackFragment extends BaseFragment {

	EditText etFeedback;
	MenuItem miSend;
	
	public FeedbackFragment() {
		super();
		tagText = ResourceUtils.getString(R.tag.fragment_feedback);
	}

	public FeedbackFragment(String tag) {
		super(tag, "");
	}

	@Override
	public int getBarTitle() {
		return R.string.feedback_name;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.feedback_name;
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	public void initComponents() {
		etFeedback = (EditText) innerView.findViewById(R.id.etFeedback);
	}

	@Override
	public void initEvents() {

	}

	@Override
	public void initLogic() {

	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.fragment_feedback;
	}

	@Override
	public String getMainActivityName() {
		return "";
	}

	@Override
	public void initMenu(Menu menu) {
		miSend = menu.add(0, MenuIds.MENU_SEND, 21, R.string.settings_send);
		miSend.setIcon(android.R.drawable.ic_menu_send);
		miSend.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case MenuIds.MENU_SEND:
			String feedback = etFeedback.getText().toString();
			if (feedback.equals("")) {
				Toast.makeText(getActivity(), R.string.settings_no_feedback,
						Toast.LENGTH_SHORT).show();
			} else {
				APIUtils.feedback(getActivity(), feedback);
				Toast.makeText(getActivity(), R.string.settings_sent,
						Toast.LENGTH_LONG).show();
				getActivity().finish();
			}
			break;
		}
		return true;
	}

	@Override
	public void onGetNewArguments(Bundle bn) {

	}

	@Override
	public Bundle getFragmentState() {
		return null;
	}

}
