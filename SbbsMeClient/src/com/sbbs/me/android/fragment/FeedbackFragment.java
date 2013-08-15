package com.sbbs.me.android.fragment;

import android.content.Loader;
import android.content.Loader.OnLoadCompleteListener;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.EditText;
import android.widget.TextView;
import android.widget.Toast;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.ResourceUtils;
import com.sbbs.me.android.R;
import com.sbbs.me.android.api.SbbsMeAPI;
import com.sbbs.me.android.api.SbbsMeLogs;
import com.sbbs.me.android.consts.MenuIds;
import com.sbbs.me.android.loader.SbbsFeedbackSender;
import com.sbbs.me.android.utils.AccountUtils;
import com.sbbs.me.android.utils.Config;

public class FeedbackFragment extends BaseFragment implements
		OnLoadCompleteListener<String> {

	EditText etFeedback;
	TextView tvStatus;

	MenuItem miSend;
	SbbsFeedbackSender sender;

	public FeedbackFragment() {
		super();
		tagText = ResourceUtils.getString(R.tag.tag_feedback_fragment);
	}

	@Override
	public int getBarTitle() {
		return R.string.feedback;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.feedback;
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	public void initComponents() {
		etFeedback = (EditText) innerView.findViewById(R.id.etFeedback);
		tvStatus = (TextView) innerView.findViewById(R.id.tvStatus);
		sender = new SbbsFeedbackSender(getActivity());
	}

	@Override
	public void initEvents() {
		sender.registerListener(0, this);
	}

	@Override
	public void initLogic() {
		setFragmentEnabled(SbbsMeAPI.isLogin());
		if (!SbbsMeAPI.isLogin()) {
			tvStatus.setText(R.string.not_login);
			tvStatus.setVisibility(View.VISIBLE);
		}
		SbbsMeAPI.writeLogT(getActivity(), SbbsMeLogs.LOG_FEEDBACK, "");
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
		miSend = menu.add(0, MenuIds.MENU_ID_SEND, 99, R.string.send);
		miSend.setIcon(android.R.drawable.ic_menu_send);
		miSend.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
		setFragmentEnabled(SbbsMeAPI.isLogin());
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case MenuIds.MENU_ID_SEND:
			String feedback = etFeedback.getText().toString();

			if (feedback.equals("")) {
				Toast.makeText(getActivity(), R.string.post_feedback_empty,
						Toast.LENGTH_LONG).show();
				return true;
			}
			sender.setFeedback(Config.getUserId(getActivity()),
					AccountUtils.getBindedEmailAddress(getActivity()), feedback);
			setFragmentEnabled(false);
			tvStatus.setVisibility(View.VISIBLE);
			sender.startLoading();
			SbbsMeAPI
					.writeLogT(getActivity(), SbbsMeLogs.LOG_FEEDBACK_SEND, "");
			break;
		}
		return true;
	}

	private void setFragmentEnabled(boolean enabled) {
		if (etFeedback != null) {
			etFeedback.setEnabled(enabled);
		}
		if (miSend != null) {
			miSend.setEnabled(enabled);
		}
	}

	@Override
	public void onGetNewArguments(Bundle bn) {

	}

	@Override
	public Bundle getFragmentState() {
		return null;
	}

	@Override
	public void onLoadComplete(Loader<String> loader, String data) {
		if (getActivity() != null) {
			tvStatus.setVisibility(View.GONE);
			setFragmentEnabled(true);
			if ((data != null) && (!data.equals(""))) {
				Toast.makeText(getActivity(), R.string.post_feedback_ok,
						Toast.LENGTH_LONG).show();
				getActivity().finish();
			} else {
				Toast.makeText(getActivity(), R.string.post_feedback_error,
						Toast.LENGTH_LONG).show();
			}
		}
	}

}
