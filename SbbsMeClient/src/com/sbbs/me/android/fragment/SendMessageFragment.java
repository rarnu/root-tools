package com.sbbs.me.android.fragment;

import android.content.Loader;
import android.content.Loader.OnLoadCompleteListener;
import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.TextView;
import android.widget.Toast;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.ResourceUtils;
import com.sbbs.me.android.R;
import com.sbbs.me.android.loader.SbbsPrivateMessageSender;

public class SendMessageFragment extends BaseFragment implements
		OnClickListener, OnLoadCompleteListener<String> {

	String user;
	ImageView ivSendMessage, ivCloseDialog;
	EditText etMessage;
	TextView tvSending;
	SbbsPrivateMessageSender sender;

	public SendMessageFragment() {
		super();
		tagText = ResourceUtils.getString(R.tag.tag_send_message_fragment);
	}

	@Override
	public int getBarTitle() {
		return R.string.message;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.message;
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	public void initComponents() {
		ivSendMessage = (ImageView) innerView.findViewById(R.id.ivSendMessage);
		ivCloseDialog = (ImageView) innerView.findViewById(R.id.ivCloseDialog);
		etMessage = (EditText) innerView.findViewById(R.id.etMessage);
		tvSending = (TextView) innerView.findViewById(R.id.tvSending);
		sender = new SbbsPrivateMessageSender(getActivity());
	}

	@Override
	public void initEvents() {
		ivSendMessage.setOnClickListener(this);
		ivCloseDialog.setOnClickListener(this);
		sender.registerListener(0, this);
	}

	@Override
	public void initLogic() {
		user = getArguments().getString("user");

	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.dialog_message;
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
		switch (v.getId()) {
		case R.id.ivCloseDialog:
			getActivity().finish();
			break;
		case R.id.ivSendMessage:
			String msg = etMessage.getText().toString();
			if (msg.equals("")) {
				Toast.makeText(getActivity(), R.string.empty_message,
						Toast.LENGTH_SHORT);
				return;
			}
			sender.setMessage(user, "Markdown", msg);
			setComponentEnabled(false);
			sender.startLoading();
			break;
		}
	}

	private void setComponentEnabled(boolean enabled) {
		etMessage.setEnabled(enabled);
		ivSendMessage.setEnabled(enabled);
		tvSending.setVisibility(enabled ? View.GONE : View.VISIBLE);
	}

	@Override
	public void onLoadComplete(Loader<String> loader, String data) {
		if (getActivity() != null) {
			setComponentEnabled(true);
			if (data.equals("OK")) {
				Toast.makeText(getActivity(), R.string.send_message_succ,
						Toast.LENGTH_SHORT).show();
				getActivity().finish();
			} else {
				Toast.makeText(getActivity(), R.string.send_message_failed,
						Toast.LENGTH_LONG).show();
			}
		}
	}

}
