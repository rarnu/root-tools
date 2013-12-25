package com.rarnu.tools.root.fragment;

import android.app.AlertDialog;
import android.content.Intent;
import android.content.Loader;
import android.net.Uri;
import android.os.Bundle;
import android.text.Html;
import android.view.Menu;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ScrollView;
import android.widget.TextView;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.component.BlockListView;
import com.rarnu.devlib.component.DataProgressBar;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.adapter.TeamBuildAdapter;
import com.rarnu.tools.root.adapter.TeamMemberAdapter;
import com.rarnu.tools.root.common.TeamBuildInfo;
import com.rarnu.tools.root.common.TeamInfo;
import com.rarnu.tools.root.common.TeamMemberInfo;
import com.rarnu.tools.root.loader.TeamLoader;
import com.rarnu.utils.UIUtils;

import java.util.ArrayList;
import java.util.List;

public class BuildTeamFragment extends BaseFragment implements View.OnClickListener, AdapterView.OnItemClickListener, Loader.OnLoadCompleteListener<TeamInfo> {

    BlockListView lvMembers;
    BlockListView lvCompile;
    TextView tvGithubLink;

    List<TeamMemberInfo> listMember;
    List<TeamBuildInfo> listBuild;
    TeamMemberAdapter adapterMember;
    TeamBuildAdapter adapterBuild;

    TeamLoader loader;
    DataProgressBar progressTeam;

    @Override
    public int getBarTitle() {
        return R.string.build_team;
    }

    @Override
    public int getBarTitleWithPath() {
        return R.string.build_team;
    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public void initComponents() {
        lvMembers = (BlockListView) innerView.findViewById(R.id.lvMembers);
        lvCompile = (BlockListView) innerView.findViewById(R.id.lvCompile);
        tvGithubLink = (TextView) innerView.findViewById(R.id.tvGithubLink);
        progressTeam = (DataProgressBar) innerView.findViewById(R.id.progressTeam);

        listMember = new ArrayList<TeamMemberInfo>();
        listBuild = new ArrayList<TeamBuildInfo>();
        adapterMember = new TeamMemberAdapter(getActivity(), listMember);
        adapterBuild = new TeamBuildAdapter(getActivity(), listBuild);
        lvMembers.setAdapter(adapterMember);
        lvCompile.setAdapter(adapterBuild);

        lvMembers.setItemHeight(UIUtils.dipToPx(56));
        lvCompile.setItemHeight(UIUtils.dipToPx(56));

        loader = new TeamLoader(getActivity());
        progressTeam.setAppName(getString(R.string.loading));
    }

    @Override
    public void initEvents() {
        tvGithubLink.setOnClickListener(this);
        lvCompile.setOnItemClickListener(this);
        loader.registerListener(0, this);
    }

    @Override
    public void initLogic() {
        progressTeam.setVisibility(View.VISIBLE);
        loader.startLoading();
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.layout_build_team;
    }

    @Override
    public String getMainActivityName() {
        return MainActivity.class.getName();
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
            case R.id.tvGithubLink:
                Intent inGithub = new Intent(Intent.ACTION_VIEW);
                inGithub.setData(Uri.parse("http://github.com/rarnu/root-tools"));
                startActivity(inGithub);
                break;
        }
    }

    @Override
    public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        // click compile step
        TeamBuildInfo item = listBuild.get(position);
        final ScrollView sv = new ScrollView(getActivity());
        final TextView tv = new TextView(getActivity());
        int padding = UIUtils.dipToPx(16);
        sv.setPadding(padding, padding, padding, padding);
        sv.addView(tv);
        tv.setText(Html.fromHtml(item.desc));
        new AlertDialog.Builder(getActivity())
                .setTitle(item.title)
                .setView(sv)
                .setPositiveButton(R.string.ok, null)
                .show();

    }

    @Override
    public void onLoadComplete(Loader<TeamInfo> loader, TeamInfo data) {
        listMember.clear();
        listBuild.clear();
        if (data != null) {
            listMember.addAll(data.listMember);
            listBuild.addAll(data.listBuild);
        }
        if (getActivity() != null) {
            adapterMember.setNewList(listMember);
            adapterBuild.setNewList(listBuild);
            lvMembers.resize();
            lvCompile.resize();
            progressTeam.setVisibility(View.GONE);
        }
    }
}
