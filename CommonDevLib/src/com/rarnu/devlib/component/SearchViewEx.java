package com.rarnu.devlib.component;

import android.content.Context;
import android.graphics.Color;
import android.util.AttributeSet;
import android.view.inputmethod.EditorInfo;
import android.widget.EditText;
import android.widget.ImageButton;
import android.widget.RelativeLayout;
import com.rarnu.devlib.R;
import com.rarnu.utils.UIUtils;

/**
 * Created by rarnu on 2/6/15.
 */
public class SearchViewEx extends RelativeLayout {

    EditText etSearch;
    ImageButton btnCancel;

    public static final int ID_BUTTON_CANCEL = 10002;

    public SearchViewEx(Context context, AttributeSet attrs, int defStyle) {
        super(context, attrs, defStyle);
        init();
    }

    public SearchViewEx(Context context, AttributeSet attrs) {
        super(context, attrs);
        init();
    }

    public SearchViewEx(Context context) {
        super(context);
        init();
    }

    private void init() {

        etSearch = new EditText(getContext());
        RelativeLayout.LayoutParams rllpEdit = new RelativeLayout.LayoutParams(LayoutParams.MATCH_PARENT, UIUtils.dipToPx(48));
        rllpEdit.addRule(RelativeLayout.CENTER_VERTICAL, RelativeLayout.TRUE);
        rllpEdit.topMargin = UIUtils.dipToPx(8);
        rllpEdit.bottomMargin = UIUtils.dipToPx(8);
        rllpEdit.leftMargin = UIUtils.dipToPx(8);
        rllpEdit.rightMargin = UIUtils.dipToPx(8);
        etSearch.setLayoutParams(rllpEdit);
        etSearch.setImeOptions(EditorInfo.IME_ACTION_SEARCH);
        etSearch.setBackgroundResource(R.drawable.bg_edittext_transparent);
        etSearch.setTextColor(Color.WHITE);
        addView(etSearch);

        btnCancel = new ImageButton(getContext());
        btnCancel.setId(ID_BUTTON_CANCEL);
        RelativeLayout.LayoutParams rllpCancel = new RelativeLayout.LayoutParams(UIUtils.dipToPx(36), UIUtils.dipToPx(36));
        rllpCancel.addRule(RelativeLayout.ALIGN_PARENT_RIGHT, RelativeLayout.TRUE);
        rllpCancel.addRule(RelativeLayout.CENTER_VERTICAL, RelativeLayout.TRUE);
        rllpCancel.rightMargin = UIUtils.dipToPx(10);
        rllpCancel.topMargin = UIUtils.dipToPx(10);
        rllpCancel.bottomMargin = UIUtils.dipToPx(10);
        btnCancel.setLayoutParams(rllpCancel);
        btnCancel.setBackgroundResource(R.drawable.btn_cab_done);
        btnCancel.setImageResource(android.R.drawable.ic_menu_close_clear_cancel);
        addView(btnCancel);

        etSearch.requestFocus();
    }

    public void setCancelButtonListener(OnClickListener listener) {
        btnCancel.setOnClickListener(listener);
    }

    public void setText(String text) {
        etSearch.setText(text);
    }

    public String getText() {
        return etSearch.getText().toString();
    }

    public EditText getEditor() {
        return etSearch;
    }

}
