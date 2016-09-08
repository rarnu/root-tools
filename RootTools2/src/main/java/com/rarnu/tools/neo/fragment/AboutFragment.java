package com.rarnu.tools.neo.fragment;

import android.content.Intent;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.net.Uri;
import android.os.Bundle;
import android.text.Html;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.TextView;
import com.rarnu.tools.neo.R;
import com.rarnu.tools.neo.base.BaseFragment;
import com.rarnu.tools.neo.utils.FileUtils;

import java.io.IOException;

public class AboutFragment extends BaseFragment implements View.OnClickListener {

    private TextView tvVersion = null;
    private TextView tvCoderStory = null;
    private TextView tvCoderStoryGithub = null;
    private TextView tvProj = null;
    private TextView tvIntro = null;

    @Override
    public int getBarTitle() {
        return R.string.about_name;
    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public void initComponents() {
        tvVersion = (TextView) innerView.findViewById(R.id.tvVersion);
        tvProj = (TextView) innerView.findViewById(R.id.tvProj);
        tvCoderStory = (TextView) innerView.findViewById(R.id.tvCoderStory);
        tvCoderStoryGithub = (TextView) innerView.findViewById(R.id.tvCoderStoryGithub);
        tvIntro = (TextView) innerView.findViewById(R.id.tvIntro);
    }

    @Override
    public void initEvents() {
        tvProj.setOnClickListener(this);
        tvCoderStory.setOnClickListener(this);
        tvCoderStoryGithub.setOnClickListener(this);
    }

    @Override
    public void initLogic() {
        String ver = "unknown";
        try {
            PackageInfo info = getContext().getPackageManager().getPackageInfo(getContext().getPackageName(), 0);
            ver = info.versionName;
        } catch (Exception e) {

        }
        tvVersion.setText(getString(R.string.view_about_version, ver));
        try {
            String intro = FileUtils.readAssetFile(getContext(), "intro");
            tvIntro.setText(intro);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.fragment_about;
    }

    @Override
    public String getMainActivityName() {
        return null;
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
            case R.id.tvProj:
                openUrl(R.string.view_about_project_github_url);
                break;
            case R.id.tvCoderStory:
                openUrl(R.string.view_about_thankto_coderstory_url);
                break;
            case R.id.tvCoderStoryGithub:
                openUrl(R.string.view_about_thankto_coderstory_github_url);
                break;
        }
    }

    private void openUrl(int resId) {
        Uri u = Uri.parse(getString(resId));
        Intent inWeb = new Intent(Intent.ACTION_VIEW);
        inWeb.setData(u);
        startActivity(inWeb);
    }
}
