package com.rarnu.tools.neo.comp;

import android.content.Context;
import android.content.res.TypedArray;
import android.preference.Preference;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.Switch;
import android.widget.TextView;
import com.rarnu.tools.neo.R;

public class PreferenceEx extends Preference {

    private RelativeLayout layPref = null;
    private ImageView prefIcon = null;
    private TextView prefTitle = null;
    private TextView prefSummary = null;
    private Switch prefStatus = null;

    private View innerView = null;
    private boolean showSwitch = false;
    private boolean showIcon = true;
    private boolean isOn = false;

    public PreferenceEx(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        initAttr(attrs);
    }

    public PreferenceEx(Context context, AttributeSet attrs) {
        super(context, attrs);
        initAttr(attrs);
    }

    public PreferenceEx(Context context) {
        super(context);
    }

    private void initAttr(AttributeSet attrs) {
        if (attrs != null) {
            TypedArray a = getContext().obtainStyledAttributes(attrs, R.styleable.PreferenceEx, 0, 0);
            showSwitch = a.getBoolean(R.styleable.PreferenceEx_showSwitch, false);
            showIcon = a.getBoolean(R.styleable.PreferenceEx_showIcon,true);
            a.recycle();
        }
    }

    @Override
    protected void onBindView(View view) {
        try {
            super.onBindView(view);
        } catch (Exception e) {

        }
        prefTitle.setText(getTitle());
        prefSummary.setText(getSummary());
        if (getSummary() == null || getSummary().equals("")) {
            prefSummary.setVisibility(View.GONE);
        }
        prefStatus.setChecked(isOn);
        prefIcon.setImageDrawable(getIcon());
    }

    @Override
    protected View onCreateView(ViewGroup parent) {
        super.onCreateView(parent);
        if (innerView == null) {
            innerView = LayoutInflater.from(getContext()).inflate(R.layout.comp_preference, parent, false);
            layPref = (RelativeLayout) innerView.findViewById(R.id.layPref);
            prefIcon = (ImageView) innerView.findViewById(R.id.prefIcon);
            prefTitle = (TextView) innerView.findViewById(R.id.prefTitle);
            prefSummary = (TextView) innerView.findViewById(R.id.prefSummary);
            prefStatus = (Switch) innerView.findViewById(R.id.prefStatus);
            prefStatus.setVisibility(showSwitch ? View.VISIBLE : View.GONE);
            prefIcon.setVisibility(showIcon ? View.VISIBLE : View.GONE);
        }
        return innerView;
    }

    @Override
    public void setTitle(int titleResId) {
        super.setTitle(titleResId);
        prefTitle.setText(titleResId);
    }

    @Override
    public void setSummary(int summaryResId) {
        super.setSummary(summaryResId);
        prefSummary.setText(summaryResId);
        if (getSummary() == null || getSummary().equals("")) {
            prefSummary.setVisibility(View.GONE);
        }
    }

    @Override
    public void setIcon(int iconResId) {
        super.setIcon(iconResId);
        prefIcon.setImageDrawable(getIcon());
    }

    public void setShowSwitch(boolean on) {
        prefStatus.setVisibility(on ? View.VISIBLE : View.GONE);
    }

    public void setShowIcon(boolean on) {
        prefIcon.setVisibility(on ? View.VISIBLE : View.GONE);
    }

    public void setStatus(boolean on) {
        isOn = on;
        if (prefStatus != null) {
            prefStatus.setChecked(on);
        }
    }

    public boolean getStatus() {
        return prefStatus.isChecked();
    }
}
