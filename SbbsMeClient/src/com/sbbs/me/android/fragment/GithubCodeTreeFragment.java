package com.sbbs.me.android.fragment;

import java.util.Iterator;
import java.util.Map;

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
import com.sbbs.me.android.api.SbbsMeCodeTree;
import com.sbbs.me.android.api.SbbsMeCodeTree.Entry;
import com.sbbs.me.android.component.BlockTextView;
import com.sbbs.me.android.loader.SbbsAndroidRepoLoader;
import com.sbbs.me.android.loader.SbbsWebRepoLoader;

public class GithubCodeTreeFragment extends BaseFragment implements 
		OnLoadCompleteListener<SbbsMeCodeTree> {
	
	RelativeLayout layTree;
	SbbsWebRepoLoader webLoader;
	SbbsAndroidRepoLoader androidLoader;
	SbbsMeCodeTree tree = null;
	TextView treeLoading;
	int type = 0;
	
	public GithubCodeTreeFragment(int type) {
		super();
		this.type = type;
		tagText = ResourceUtils
				.getString(type == 0 ? R.tag.tag_ongithub_fragment_sbbs
						: R.tag.tag_ongithub_fragment_android);
		tabTitle = ResourceUtils.getString(type == 0 ? R.string.project_sbbs_me
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
		layTree = (RelativeLayout) innerView.findViewById(R.id.layTree);
		treeLoading = (TextView) innerView.findViewById(R.id.treeLoading);
		if (type == 0) {
			androidLoader = new SbbsAndroidRepoLoader(getActivity());
		} else if (type == 1) {
			webLoader = new SbbsWebRepoLoader(getActivity());
		}
	}

	@Override
	public void initEvents() {
		if (type == 0) {
			androidLoader.registerListener(0, this);
		} else if (type == 1) {
			webLoader.registerListener(0, this);
		}
		
	}

	@Override
	public void initLogic() {
		treeLoading.setVisibility(View.VISIBLE);
		if (type == 0) {
			androidLoader.startLoading();
		} else if (type == 1) {
			webLoader.startLoading();
		}
	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.fragment_codetree;
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
	public void onLoadComplete(Loader<SbbsMeCodeTree> loader, SbbsMeCodeTree root) {
		tree = root;
		if (tree != null) {
			buildUI();
		}
		treeLoading.setVisibility(View.GONE);
	}

	private void buildUI() {
		int viewId = 100000;
		if (getActivity() != null) {
			Map<String, Entry> files = tree.root.files;
			Iterator<java.util.Map.Entry<String, Entry>> it = files.entrySet().iterator();
			while ( it.hasNext() ) {
				Map.Entry<String, Entry> entry = (Map.Entry<String, Entry>)it.next();
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
				block.setText(entry.getKey()+" "+entry.getValue().entry.getSize());
				layTree.addView(block);
				layTree.postInvalidate();
				viewId++;
			}
		}
	}
}
