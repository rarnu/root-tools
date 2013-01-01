package com.rarnu.tools.root.fragment;

import android.os.Handler;
import android.os.Message;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.widget.EditText;
import android.widget.Toast;

import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.api.LogApi;
import com.rarnu.tools.root.api.MobileApi;
import com.rarnu.tools.root.base.BaseFragment;
import com.rarnu.tools.root.base.MenuItemIds;
import com.rarnu.tools.root.comp.DataProgressBar;

public class FeedbackFragment extends BaseFragment {

	EditText etFeedback;
	DataProgressBar progressFeedback;
	MenuItem itemSend;

	@Override
	public void onCreateOptionsMenu(Menu menu, MenuInflater inflater) {
		menu.clear();
		itemSend = menu.add(0, MenuItemIds.MENU_SEND, 99, R.string.send);
		itemSend.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
		itemSend.setIcon(android.R.drawable.ic_menu_send);
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		if (item.getItemId() == MenuItemIds.MENU_SEND) {
			String comment = etFeedback.getText().toString();
			if (comment.equals("")) {
				Toast.makeText(getActivity(), R.string.empty_feedback,
						Toast.LENGTH_LONG).show();
				return true;
			}
			doSendFeedbackT(comment);
		}
		return true;
	}

	private void doSendFeedbackT(final String comment) {
		progressFeedback.setAppName(getString(R.string.sending));
		progressFeedback.setVisibility(View.VISIBLE);
		etFeedback.setEnabled(false);
		itemSend.setEnabled(false);
		LogApi.logUserFeedback();
		final Handler h = new Handler() {

			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					//
					if (msg.arg1 != 0) {
						// succ
						Toast.makeText(getActivity(),
								R.string.send_feedback_succ, Toast.LENGTH_LONG)
								.show();
						etFeedback.setText("");

					} else {
						// fail
						Toast.makeText(getActivity(),
								R.string.send_feedback_fail, Toast.LENGTH_LONG)
								.show();
					}
					progressFeedback.setVisibility(View.GONE);
					itemSend.setEnabled(true);
					etFeedback.setEnabled(true);
				}
				super.handleMessage(msg);
			}

		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				boolean ret = MobileApi.userFeedback(GlobalInstance.deviceId,
						GlobalInstance.module, GlobalInstance.osVersion,
						GlobalInstance.mail, GlobalInstance.buildDescription,
						comment);
				Message msg = new Message();
				msg.what = 1;
				msg.arg1 = (ret ? 1 : 0);
				h.sendMessage(msg);

			}
		}).start();

	}

	@Override
	protected int getBarTitle() {
		return R.string.feedback;
	}

	@Override
	protected int getBarTitleWithPath() {
		return R.string.feedback_with_path;
	}

	@Override
	protected void initComponents() {
		etFeedback = (EditText) innerView.findViewById(R.id.etFeedback);
		progressFeedback = (DataProgressBar) innerView
				.findViewById(R.id.progressFeedback);
		
	}

	@Override
	protected int getFragmentLayoutResId() {
		return R.layout.layout_user_feedback;
	}
}
