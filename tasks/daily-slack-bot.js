const https = require("https");

const workflowPath =
    "/repos/superfluid-finance/protocol-monorepo/actions/runs?per_page=100";
const pullRequestPath =
    "/repos/superfluid-finance/protocol-monorepo/pulls?state=open";

const warningIcon =
    "https://api.slack.com/img/blocks/bkb_template_images/notificationsWarningIcon.png";
const greenCheckMark =
    "https://emojipedia-us.s3.amazonaws.com/source/skype/289/check-mark-button_2705.png";
const redWarningIcon =
    "https://uxwing.com/wp-content/themes/uxwing/download/26-signs-symbols/red-alert.png";
const topSectionMessage =
    "Looks like there are some lonely pull requests open in your area";
const workflowFileName = ".github/workflows/ci.canary.yml";

const redImage =
    "https://upload.wikimedia.org/wikipedia/commons/thumb/6/62/Solid_red.svg/512px-Solid_red.svg.png?20150316143248";
const orangeImage =
    "https://5.imimg.com/data5/TK/YH/MY-451313/yellowish-orange-dye-500x500.jpg";
const greenImage =
    "https://upload.wikimedia.org/wikipedia/commons/thumb/3/32/Auto_Racing_Green.svg/1280px-Auto_Racing_Green.svg.png";

async function getDataAsJson(path) {
    let options = {
        headers: {
            "Content-Type": "application/json",
            "User-Agent": "Elvi.js slack bot",
        },
        hostname: "api.github.com",
        path: path,
        method: "GET",
    };

    return new Promise((resolve) => {
        const req = https.request(options, (res) => {
            let body = "";
            res.on("data", function (chunk) {
                body += chunk;
            });
            res.on("end", function () {
                resolve(JSON.parse(body));
            });
        });

        req.on("error", (err) => {
            console.log(err);
        });
        req.end();
    });
}

async function sendMessageToSlack(data) {
    const slackHostName = "hooks.slack.com";
    let topSecret = process.argv[2].split(slackHostName)[1];
    let options = {
        headers: {
            "Content-Type": "application/json",
            "User-Agent": "Elvi.js slack bot",
        },
        hostname: slackHostName,
        path: topSecret,
        method: "POST",
    };

    const req = https
        .request(options, (res) => {
            console.log("Status Code:", res.statusCode);

            res.on("data", (chunk) => {
                data += chunk;
            });
        })
        .on("error", (err) => {
            console.log("Error: ", err.message);
        });

    req.write(data);
    req.end();
}

(async () => {
    const prJson = await getDataAsJson(pullRequestPath);
    const workflowJson = await getDataAsJson(workflowPath);
    const amountOfPRsOpen = prJson.length;
    const oldestOpenPR = prJson[prJson.length - 1];
    const oldestOpenPRTitle = oldestOpenPR.title;
    const oldestPRAuthorName = oldestOpenPR.user.login;
    const oldestPRAuthorPicture = oldestOpenPR.user.avatar_url;
    const oldestPRCreatedByUrl = oldestOpenPR.user.url;
    const oldestPRUrl = oldestOpenPR.html_url;

    const lastWorkflow = workflowJson.workflow_runs.filter(
        (x) => x.path === workflowFileName
    )[0];
    const lastWorkflowId = lastWorkflow.id;
    const lastWorkflowUsage = await getDataAsJson(
        "/repos/superfluid-finance/protocol-monorepo/actions/runs/" +
            lastWorkflowId +
            "/timing"
    );

    const workflowStatus = lastWorkflow.status;
    const workflowConclusion = lastWorkflow.conclusion;
    const workflowRanAt = new Date(lastWorkflow.run_started_at).toUTCString();
    const workflowUrl = lastWorkflow.html_url;
    const workflowNumber = lastWorkflow.run_number;
    const workflowName = lastWorkflow.name;

    const workflowTriggeringCommitMessage =
        lastWorkflow.head_commit.message.split("\n")[0];
    const workflowCommitLink =
        "https://github.com/superfluid-finance/protocol-monorepo/commit/" +
        lastWorkflow.id;

    let webhookPayload = { blocks: [] };

    async function getOldestPrOldestCommit(prJson) {
        let allCommits = await getDataAsJson(
            "/repos/superfluid-finance/protocol-monorepo/pulls/" +
                prJson.number +
                "/commits"
        );
        return allCommits[allCommits.length - 1];
    }

    let olderstPrOldestCommit = await getOldestPrOldestCommit(oldestOpenPR);
    const oldestPRLastUpdate = new Date(
        olderstPrOldestCommit.commit.author.date
    );

    const oldestPRMessage = olderstPrOldestCommit.commit.message;

    const msInADay = 1000 * 60 * 60 * 24;
    const lastUpdatedBeforeDays = (
        (Date.now() - oldestPRLastUpdate) /
        msInADay
    ).toFixed(0);

    function convertMS(ms) {
        var d, h, m, s;
        s = Math.floor(ms / 1000);
        m = Math.floor(s / 60);
        s = s % 60;
        h = Math.floor(m / 60);
        m = m % 60;
        d = Math.floor(h / 24);
        h = h % 24;
        h += d * 24;
        return h + ":" + m + ":" + s;
    }

    function addHeader(payload, text) {
        let header = {
            type: "header",
            text: {
                type: "plain_text",
                text: text,
                emoji: true,
            },
        };
        payload.blocks.push(header);
    }

    function addPlainText(payload, text) {
        let header = {
            type: "section",
            text: {
                type: "plain_text",
                text: text,
                emoji: true,
            },
        };
        payload.blocks.push(header);
    }

    function addSectionWithImage(payload, text, image, imageText) {
        let section = {
            type: "section",
            text: {
                type: "mrkdwn",
                text: text,
            },
            accessory: {
                type: "image",
                image_url: image,
                alt_text: imageText,
            },
        };
        payload.blocks.push(section);
    }

    function addContextWithImage(payload, text, image, imageText) {
        let context = {
            type: "context",
            elements: [
                {
                    type: "image",
                    image_url: image,
                    alt_text: imageText,
                },
                {
                    type: "mrkdwn",
                    text: text,
                },
            ],
        };
        payload.blocks.push(context);
    }

    function addDivider(payload) {
        let divider = {
            type: "divider",
        };
        payload.blocks.push(divider);
    }

    function getAssigneeString() {
        let finalString = "Asigned to: ";
        oldestOpenPR.assignees.forEach((asignee) => {
            finalString += "*<" + asignee.url + "|" + asignee.login + ">*,";
        });
        if (oldestOpenPR.assignees.length > 0) {
            return finalString.slice(0, -1) + " please have a look\n";
        } else {
            return "Nobody is assigned to this PR, we need to find someone to shame ASAP\n";
        }
    }

    function getLastPRString() {
        return "*<" + oldestPRUrl + "|" + oldestOpenPRTitle + ">*" + "\n";
    }

    function getPRCreatedByString() {
        return (
            "Created by: *<" +
            oldestPRCreatedByUrl +
            "|" +
            oldestPRAuthorName +
            ">*\n"
        );
    }

    function getPRAmountString() {
        return (
            "*<https://github.com/superfluid-finance/protocol-monorepo/pulls|There are " +
            amountOfPRsOpen +
            " PRs open in total>*"
        );
    }

    function addPRSection() {
        let PRString =
            getLastPRString() +
            getAssigneeString() +
            getPRCreatedByString() +
            getPRAmountString();
        addSectionWithImage(
            webhookPayload,
            PRString,
            oldestPRAuthorPicture,
            oldestPRAuthorName
        );
        addPRContext();
    }

    function addPRContext() {
        let imageToAddToContext;
        let imageText;
        if (lastUpdatedBeforeDays >= 30) {
            imageToAddToContext = redWarningIcon;
            imageText =
                "C'mon guys it has been a month already , lets move this along";
        } else {
            imageToAddToContext =
                lastUpdatedBeforeDays < 14 ? greenCheckMark : warningIcon;

            imageText =
                lastUpdatedBeforeDays < 14
                    ? "Please, publicly shame Elvijs if this value is wrong ,otherwise the PR is nice and fresh"
                    : "Amigo, the PR is hanging there for more than 2 weeks already, maybe have a look?";
        }
        addContextWithImage(
            webhookPayload,
            "*The PR has been last updated before " +
                lastUpdatedBeforeDays +
                " days*\nLast commit: " +
                oldestPRMessage,
            imageToAddToContext,
            imageText
        );
        addDivider(webhookPayload);
    }

    function getOverallWorkflowString() {
        if (workflowStatus === "in_progress") {
            return "In progress";
        } else {
            return workflowConclusion === "success"
                ? "Success"
                : "Failed";
        }
    }

    function getWorkflowString() {
        return (
            workflowName +
            " *<" +
            workflowUrl +
            "|#" +
            workflowNumber +
            ">*: " +
            workflowConclusion +
            "\nLast commit: *<" +
            workflowCommitLink +
            "|" +
            workflowTriggeringCommitMessage +
            ">*\nWorkflow ran at: " +
            workflowRanAt +
            "\n" +
            workflowName +
            " ran for: " +
            convertMS(lastWorkflowUsage.run_duration_ms)
        );
    }

    function getWorkflowPicture() {
        if (workflowStatus === "in_progress") {
            return orangeImage;
        } else {
            return workflowConclusion === "success"
                ? greenImage
                : redImage;
        }
    }

    function addWorkflowSection() {
        addSectionWithImage(
            webhookPayload,
            getWorkflowString(),
            getWorkflowPicture(),
            "Sorry if you are color blind"
        );
    }

    addHeader(webhookPayload, "Elvi.js protocol monorepo public shamer");
    addPlainText(webhookPayload, topSectionMessage);
    addDivider(webhookPayload);
    addPRSection();
    addHeader(
        webhookPayload,
        workflowName +
            " latest status: " +
            getOverallWorkflowString()
    );
    addWorkflowSection();
    addDivider(webhookPayload);
    await sendMessageToSlack(JSON.stringify(webhookPayload));
})();