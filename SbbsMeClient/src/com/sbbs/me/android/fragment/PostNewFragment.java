package com.sbbs.me.android.fragment;

import android.app.Activity;
import android.content.Intent;
import android.content.Loader;
import android.content.Loader.OnLoadCompleteListener;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.EditText;
import android.widget.TextView;
import android.widget.Toast;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.ResourceUtils;
import com.sbbs.me.android.GalleryActivity;
import com.sbbs.me.android.Global;
import com.sbbs.me.android.IMainIntf;
import com.sbbs.me.android.R;
import com.sbbs.me.android.api.SbbsMeAPI;
import com.sbbs.me.android.consts.MenuIds;
import com.sbbs.me.android.loader.SbbsArticleSender;

public class PostNewFragment extends BaseFragment implements
		OnLoadCompleteListener<String>, OnClickListener {

	MenuItem miSend;
	Button btnPublic;
	Button btnFormat;
	Button btnAddImage;

	String currentFormat = "Markdown";
	boolean currentPublicMode = true;

	SbbsArticleSender sender;
	TextView tvStatus;
	EditText etSubject, etTags, etContent;

	public PostNewFragment() {
		super();
		tagText = ResourceUtils.getString(R.tag.tag_postnew_fragment);
	}

	@Override
	public int getBarTitle() {
		return R.string.lm_postnew;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.lm_postnew;
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	public void initComponents() {
		etSubject = (EditText) innerView.findViewById(R.id.etSubject);
		etTags = (EditText) innerView.findViewById(R.id.etTags);
		etContent = (EditText) innerView.findViewById(R.id.etContent);
		tvStatus = (TextView) innerView.findViewById(R.id.tvStatus);
		btnFormat = (Button) innerView.findViewById(R.id.btnFormat);
		btnPublic = (Button) innerView.findViewById(R.id.btnPublic);
		btnAddImage = (Button) innerView.findViewById(R.id.btnAddImage);

		sender = new SbbsArticleSender(getActivity());

	}

	@Override
	public void initEvents() {
		sender.registerListener(0, this);
		btnFormat.setOnClickListener(this);
		btnPublic.setOnClickListener(this);
		btnAddImage.setOnClickListener(this);
	}

	@Override
	public void initLogic() {
		setFragmentEnabled(SbbsMeAPI.isLogin());
		if (!SbbsMeAPI.isLogin()) {
			tvStatus.setText(R.string.not_login);
			tvStatus.setVisibility(View.VISIBLE);
		}
	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.fragment_postnew;
	}

	@Override
	public String getMainActivityName() {
		return "";
	}

	@Override
	public void initMenu(Menu menu) {
		miSend = menu.add(0, MenuIds.MENU_ID_SEND, 99, R.string.send);
		miSend.setIcon(android.R.drawable.ic_menu_send);
		miSend.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);

		setFragmentEnabled(SbbsMeAPI.isLogin());
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case MenuIds.MENU_ID_SEND:
			String subject = etSubject.getText().toString();
			String tags = etTags.getText().toString();
			String content = etContent.getText().toString();
			if (subject.equals("") || content.equals("")) {

				Toast.makeText(getActivity(), R.string.post_check_empty,
						Toast.LENGTH_LONG).show();
				return true;
			}
			sender.setData(subject, tags, content, currentFormat,
					currentPublicMode);
			setFragmentEnabled(false);
			tvStatus.setVisibility(View.VISIBLE);
			sender.startLoading();
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

	private void setFragmentEnabled(boolean enabled) {
		if (etSubject != null) {
			etSubject.setEnabled(enabled);
			etTags.setEnabled(enabled);
			etContent.setEnabled(enabled);
			btnAddImage.setEnabled(enabled);
			btnFormat.setEnabled(enabled);
			btnPublic.setEnabled(enabled);
		}
		if (miSend != null) {
			miSend.setEnabled(enabled);
		}
	}

	@Override
	public void onLoadComplete(Loader<String> loader, String data) {
		if (getActivity() != null) {
			tvStatus.setVisibility(View.GONE);
			setFragmentEnabled(true);
			if ((data != null) && (data.equals("") || data.length() == 24)) {
				Global.autoRefreshTag = true;
				((IMainIntf) getActivity()).switchPage(0, false);
			} else {
				Toast.makeText(getActivity(), R.string.post_error,
						Toast.LENGTH_LONG).show();
			}
		}
	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnFormat:
			currentFormat = currentFormat.equals("Markdown") ? "HTML"
					: "Markdown";
			btnFormat.setText(currentFormat);
			break;
		case R.id.btnPublic:
			currentPublicMode = !currentPublicMode;
			btnPublic.setText(currentPublicMode ? R.string.post_public
					: R.string.post_private);
			break;
		case R.id.btnAddImage:
			startActivityForResult(new Intent(getActivity(),
					GalleryActivity.class).putExtra("select_mode", true), 0);
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
			if (currentFormat.equals("Markdown")) {
				etContent.getText().insert(
						etContent.getSelectionStart(),
						String.format("![](%s%s)", SbbsMeAPI.ROOT_URL,
								data.getStringExtra("image")));
			} else {
				etContent.getText().insert(
						etContent.getSelectionStart(),
						String.format("<img src=\"%s%s\" />",
								SbbsMeAPI.ROOT_URL,
								data.getStringExtra("image")));
			}
			break;
		}
	}

}
