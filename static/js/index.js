Survey
    .StylesManager
    .applyTheme("modern");

function doOnCurrentPageChanged(survey) {
    document
        .getElementById('pageSelector')
        .value = survey.currentPageNo;
    document
        .getElementById('surveyPrev')
        .style
        .display = !survey.isFirstPage
            ? "inline"
            : "none";
    document
        .getElementById('surveyNext')
        .style
        .display = !survey.isLastPage
            ? "inline"
            : "none";
    document
        .getElementById('surveyComplete')
        .style
        .display = survey.isLastPage
            ? "inline"
            : "none";
    document
        .getElementById('surveyProgress')
        .innerText = "Page " + (
    survey.currentPageNo + 1) + " of " + survey.visiblePageCount + ".";
    if (document.getElementById('surveyPageNo')) 
        document
            .getElementById('surveyPageNo')
            .value = survey.currentPageNo;
    }
function setupPageSelector(survey) {
    var selector = document.getElementById('pageSelector');
    for (var i = 0; i < survey.visiblePages.length; i++) {
        var option = document.createElement("option");
        option.value = i;
        option.text = "Page " + (
        i + 1);
        selector.add(option);
    }
}

var json = {
    title: "Corona virus survey.",
    pages: [
        {
            title: "How old are you?",
            questions: [
                {
                    type: "checkbox",
                    name: "age",
                    title: "Please select from the list",
                    hasOther: false,
                    isRequired: true,
                    choices: ["Under 40", "40-50", "51-60","61-70","71-80", "Over 80"]
                }
            ]
        }, {
            title: "What is your current living situation?",
            questions: [
                {
                    type: "checkbox",
                    name: "living",
                    title: "Please select from the list",
                    colCount: 4,
                    isRequired: true,
                    choices: [
                        "Living alone",
                        "Living together with family, in a shared flat, or in a supervised community facility"
                    ]
                }
            ]
        }, {
            title: "At least once a week, do you privately care for people with age-related conditions, chronic illnesses, or frailty?",
            questions: [
                {
                    type: "checkbox",
                    name: "care",
                    title: "Care services or support that you provide in connection with your professional activity isn't meant.",
                    colCount: 4,
                    isRequired: true,
                    choices: [
                        "Yes",
                        "No"
                    ]
                }
            ]
        }, {
            title: "Do you work in one of the following areas?",
            questions: [
                {
                    type: "checkbox",
                    name: "work",
                    title: "Please select from the list",
                    colCount: 4,
                    isRequired: true,
                    choices: [
                        "Medical field",
                        "In a community facility(school, day care center, university, home etc.)",
                        "No, in none of the above"
                    ]
                }
            ]
        }, {
            title: "Do you smoke?",
            questions: [
                {
                    type: "checkbox",
                    name: "smoke",
                    title: "Please select from the list",
                    colCount: 4,
                    isRequired: true,
                    choices: [
                        "Yes",
                        "No"
                    ]
                }
            ]
        }, {
            title: "Are you pregnant?",
            questions: [
                {
                    type: "checkbox",
                    name: "pregnant",
                    title: "Please select from the list",
                    colCount: 4,
                    isRequired: true,
                    choices: [
                        "Yes",
                        "No",
                        "I don't know"
                    ]
                }
            ]
        }, {
            title: "Have you had close contact with a confirmed case?",
            questions: [
                {
                    type: "checkbox",
                    name: "contact",
                    title: "Close contact with a confirmed case means: Face-to-face contact for longer than 15 minutes, direct, physical contact (touching, shaking hands, kissing), being within 1.5 meters of the person for more than 15 minutes, contact with or exchange of body fluids and/or living in the same apartment. Choose No if you have worn adequate protective measures (mask, smock) on contact.",
                    colCount: 4,
                    isRequired: true,
                    choices: [
                        "Yes",
                        "No"
                    ]
                }
            ]
        }, {
            title: "In the past 24 hours, have you had a fever (over 38°C)?",
            questions: [
                {
                    type: "checkbox",
                    name: "fever",
                    title: "Please select from the list",
                    colCount: 4,
                    isRequired: true,
                    choices: [
                        "Yes",
                        "No"
                    ]
                }
            ]
        }, {
            title: "In the past 4 days, have you had a fever (over 38°C)?",
            questions: [
                {
                    type: "checkbox",
                    name: "fever4",
                    title: "Please select from the list",
                    colCount: 4,
                    isRequired: true,
                    choices: [
                        "Yes",
                        "No"
                    ]
                }
            ]
        }, {
            title: "In the past 24 hours, which of the following symptoms have you had? (multiple selection possible)",
            questions: [
                {
                    type: "checkbox",
                    name: "symptoms",
                    title: "The question relates to acute or exacerbated symptoms and excludes chronic complaints or seasonal or allergic complaints. If you have a chronic illness, compare your current symptoms with your previous problems to answer the question. If you don't have any of the symptoms, don't select an entry and continue by choosing the Next button.",
                    colCount: 4,
                    isRequired: false,
                    choices: [
                        "Chills",
                        "Body aches",
                        "Loss of taste or smell"
                    ]
                }
            ]
        }, {
            title: "In the past 24 hours, which of the following symptoms have you had? (multiple selection possible)",
            questions: [
                {
                    type: "checkbox",
                    name: "symptomsmore",
                    title: "If you don't have any of the symptoms, don't select an entry and continue by choosing the Next button.",
                    colCount: 4,
                    isRequired: false,
                    choices: [
                        "Feeling tired or weak",
                        "Persistent cough",
                        "Runny nose",
                        "Diarrhea",
                        "Sore throat",
                        "Headache"
                    ]
                }
            ]
        }, {
            title: "In the past 24 hours, did you feel that you were more quickly out of breath than usual?",
            questions: [
                {
                    type: "checkbox",
                    name: "breath",
                    title: "Choose Yes if you: Become breathless faster than usual or have difficulty breathing with light loads, such as a walk or climbing a short flight of stairs, experience difficulty breathing or shortness of breath when sitting or lying down, have a feeling of breathlessness/shortness of breath when getting up from bed or a chair. If you have chronic lung disease, compare your current breathing problems with your existing breathing problems.",
                    colCount: 4,
                    isRequired: true,
                    choices: [
                        "Yes",
                        "No"
                    ]
                }
            ]
        }, {
            title: "With regard to all questions about symptoms: since when have you had the symptoms you specified?",
            questions: [
                {
                    type: "text",
                    name: "day",
                    title: "Day:"
                }, {
                    type: "text",
                    name: "month",
                    title: "Month:"
                }, {
                    type: "text",
                    name: "year",
                    title: "Year:"
                }
            ]
        } , {
            title: "Have you been diagnosed with chronic lung disease by a doctor?",
            questions: [
                {
                    type: "checkbox",
                    name: "lung",
                    title: "Please select from the list",
                    colCount: 4,
                    isRequired: true,
                    choices: [
                        "Yes",
                        "No",
                        "I don't know"
                    ]
                }
            ]
        }, {
            title: "Have you been diagnosed with diabetes by a doctor?",
            questions: [
                {
                    type: "checkbox",
                    name: "diabetes",
                    title: "Please select from the list",
                    colCount: 4,
                    isRequired: true,
                    choices: [
                        "Yes",
                        "No",
                        "I don't know"
                    ]
                }
            ]
        }, {
            title: "Have you been diagnosed with heart disease by a doctor?",
            questions: [
                {
                    type: "checkbox",
                    name: "heart",
                    title: "Please select from the list",
                    colCount: 4,
                    isRequired: true,
                    choices: [
                        "Yes",
                        "No",
                        "I don't know"
                    ]
                }
            ]
        }, {
            title: "Have you been diagnosed with obesity by a doctor?",
            questions: [
                {
                    type: "checkbox",
                    name: "obesity",
                    title: "Please select from the list",
                    colCount: 4,
                    isRequired: true,
                    choices: [
                        "Yes",
                        "No",
                        "I don't know"
                    ]
                }
            ]
        }, {
            title: "Are you currently taking steroids?",
            questions: [
                {
                    type: "checkbox",
                    name: "steroids",
                    title: "Please select from the list",
                    colCount: 4,
                    isRequired: true,
                    choices: [
                        "Yes",
                        "No",
                        "I don't know"
                    ]
                }
            ]
        }, {
            title: "Are you currently taking immunosuppressants?",
            questions: [
                {
                    type: "checkbox",
                    name: "immunosuppressants",
                    title: "You take or get immunosuppresives after an organ transplant, in the therapy of an autoimmune disease, or during chemotherapy.",
                    colCount: 4,
                    isRequired: true,
                    choices: [
                        "Yes",
                        "No",
                        "I don't know"
                    ]
                }
            ]
        }, {
            title: "Have you been vaccinated against flu between October 2019 and today?",
            questions: [
                {
                    type: "checkbox",
                    name: "vaccinated",
                    title: "You take or get immunosuppresives after an organ transplant, in the therapy of an autoimmune disease, or during chemotherapy.",
                    colCount: 4,
                    isRequired: true,
                    choices: [
                        "Yes",
                        "No"
                    ]
                }
            ]
        }
    ]
};

window.survey = new Survey.Model(json);

survey
    .onComplete
    .add(function (result) {
        document
            .querySelector('#surveyResult')
            .textContent = "Result JSON:\n" + JSON.stringify(result.data, null, 3);
    });

survey.showTitle = false;

$("#surveyElement").Survey({model: survey, onCurrentPageChanged: doOnCurrentPageChanged});

setupPageSelector(survey);
doOnCurrentPageChanged(survey);
survey.showTitle = false;
