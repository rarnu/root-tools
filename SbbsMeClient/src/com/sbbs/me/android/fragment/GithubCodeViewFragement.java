package com.sbbs.me.android.fragment;

import org.eclipse.egit.github.core.Blob;
import org.eclipse.egit.github.core.util.EncodingUtils;

import android.content.Loader;
import android.content.Loader.OnLoadCompleteListener;
import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.widget.LinearLayout;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.ResourceUtils;
import com.rarnu.utils.UIUtils;
import com.sbbs.me.android.R;
import com.sbbs.me.android.component.BlockTextView;
import com.sbbs.me.android.loader.SbbsCodeViewLoader;

public class GithubCodeViewFragement extends BaseFragment implements
		OnLoadCompleteListener<Blob> {

	RelativeLayout layBlob;
	SbbsCodeViewLoader loader;
	Blob blob = null;
	TextView blobLoading;
	byte repoType = 0;
	String sha;

	public GithubCodeViewFragement(byte repoType, String sha) {
		super();
		this.repoType = repoType;
		this.sha = sha;
		tagText = ResourceUtils
				.getString(repoType == 0 ? R.tag.tag_codeview_fragment_sbbs
						: R.tag.tag_codeview_fragment_android);
		tabTitle = ResourceUtils
				.getString(repoType == 0 ? R.string.project_sbbs_me
						: R.string.project_android);
	}

	@Override
	public int getBarTitle() {
		return R.string.lm_ongithub;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.lm_ongithub;
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	public void initComponents() {
		layBlob = (RelativeLayout) innerView.findViewById(R.id.layBlob);
		blobLoading = (TextView) innerView.findViewById(R.id.blobLoading);
		loader = new SbbsCodeViewLoader(getActivity(), repoType, sha);
	}

	@Override
	public void initEvents() {
		loader.registerListener(0, this);
	}

	@Override
	public void initLogic() {
		blobLoading.setVisibility(View.VISIBLE);
		loader.startLoading();
	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.fragment_codeview;
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
	public void onLoadComplete(Loader<Blob> loader, Blob data) {
		blob = data;
		if (getActivity() != null) {
			if (blob != null) {
				buildUI();
			}
			blobLoading.setVisibility(View.GONE);
		}
	}

	private void buildUI() {
		int viewId = 100000;
		if (getActivity() != null) {
			BlockTextView block = new BlockTextView(getActivity());
			block.setId(viewId);
			RelativeLayout.LayoutParams rllp = new RelativeLayout.LayoutParams(
					LinearLayout.LayoutParams.MATCH_PARENT,
					LinearLayout.LayoutParams.WRAP_CONTENT);
			if (viewId > 100000) {
				rllp.addRule(RelativeLayout.BELOW, viewId - 1);
			}
			rllp.bottomMargin = UIUtils.dipToPx(4);
			block.setLayoutParams(rllp);
			String content = blob.getContent();
			if (content == null)
				content = "";
			byte[] contents = EncodingUtils.fromBase64(content);
			block.setCodeContent(new String(contents));
			layBlob.addView(block);
			layBlob.postInvalidate();
			viewId++;
		}
	}
}
