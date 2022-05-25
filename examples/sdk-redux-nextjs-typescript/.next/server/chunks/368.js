"use strict";
exports.id = 368;
exports.ids = [368];
exports.modules = {

/***/ 7368:
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   "sfApi": () => (/* binding */ sfApi),
/* harmony export */   "sfTransactions": () => (/* binding */ sfTransactions),
/* harmony export */   "makeStore": () => (/* binding */ makeStore),
/* harmony export */   "useAppDispatch": () => (/* binding */ useAppDispatch),
/* harmony export */   "useAppSelector": () => (/* binding */ useAppSelector),
/* harmony export */   "wrapper": () => (/* binding */ wrapper)
/* harmony export */ });
/* harmony import */ var _reduxjs_toolkit__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(5184);
/* harmony import */ var _reduxjs_toolkit__WEBPACK_IMPORTED_MODULE_0___default = /*#__PURE__*/__webpack_require__.n(_reduxjs_toolkit__WEBPACK_IMPORTED_MODULE_0__);
/* harmony import */ var _superfluid_finance_sdk_redux__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(7420);
/* harmony import */ var _superfluid_finance_sdk_redux__WEBPACK_IMPORTED_MODULE_1___default = /*#__PURE__*/__webpack_require__.n(_superfluid_finance_sdk_redux__WEBPACK_IMPORTED_MODULE_1__);
/* harmony import */ var react_redux__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(6022);
/* harmony import */ var react_redux__WEBPACK_IMPORTED_MODULE_2___default = /*#__PURE__*/__webpack_require__.n(react_redux__WEBPACK_IMPORTED_MODULE_2__);
/* harmony import */ var _superfluid_finance_sdk_core__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(300);
/* harmony import */ var _superfluid_finance_sdk_core__WEBPACK_IMPORTED_MODULE_3___default = /*#__PURE__*/__webpack_require__.n(_superfluid_finance_sdk_core__WEBPACK_IMPORTED_MODULE_3__);
/* harmony import */ var ethers__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(1982);
/* harmony import */ var ethers__WEBPACK_IMPORTED_MODULE_4___default = /*#__PURE__*/__webpack_require__.n(ethers__WEBPACK_IMPORTED_MODULE_4__);
/* harmony import */ var next_redux_wrapper__WEBPACK_IMPORTED_MODULE_5__ = __webpack_require__(5648);
/* harmony import */ var next_redux_wrapper__WEBPACK_IMPORTED_MODULE_5___default = /*#__PURE__*/__webpack_require__.n(next_redux_wrapper__WEBPACK_IMPORTED_MODULE_5__);






const { sfApi  } = (0,_superfluid_finance_sdk_redux__WEBPACK_IMPORTED_MODULE_1__.initializeSfApiSlice)((options)=>(0,_superfluid_finance_sdk_redux__WEBPACK_IMPORTED_MODULE_1__.createApiWithReactHooks)({
        ...options,
        extractRehydrationInfo (action, { reducerPath  }) {
            if (action.type === next_redux_wrapper__WEBPACK_IMPORTED_MODULE_5__.HYDRATE) {
                return action.payload[reducerPath];
            }
        }
    })
);
const { sfTransactions  } = (0,_superfluid_finance_sdk_redux__WEBPACK_IMPORTED_MODULE_1__.initializeSfTransactionSlice)();
const makeStore = ()=>{
    const chainId = 5;
    (0,_superfluid_finance_sdk_redux__WEBPACK_IMPORTED_MODULE_1__.setFrameworkForSdkRedux)(chainId, ()=>_superfluid_finance_sdk_core__WEBPACK_IMPORTED_MODULE_3__.Framework.create({
            chainId,
            provider: new ethers__WEBPACK_IMPORTED_MODULE_4__.ethers.providers.InfuraProvider(chainId, "09ff473ff79949b8aa70f83613b7b09c")
        })
    );
    return (0,_reduxjs_toolkit__WEBPACK_IMPORTED_MODULE_0__.configureStore)({
        reducer: {
            sfApi: sfApi.reducer,
            sfTransactions: sfTransactions.reducer
        },
        middleware: (getDefaultMiddleware)=>getDefaultMiddleware().concat(sfApi.middleware)
    });
};
// eslint-disable-next-line @typescript-eslint/explicit-module-boundary-types
const useAppDispatch = ()=>(0,react_redux__WEBPACK_IMPORTED_MODULE_2__.useDispatch)()
;
const useAppSelector = react_redux__WEBPACK_IMPORTED_MODULE_2__.useSelector;
// NOTE: The serialization is important to override because RTK-Query will have some "undefined" values in the state which Next.js doesn't like to serialize by default.
const wrapper = (0,next_redux_wrapper__WEBPACK_IMPORTED_MODULE_5__.createWrapper)(makeStore, {
    debug: true,
    serializeState: (state)=>JSON.stringify(state)
    ,
    deserializeState: (state)=>JSON.parse(state)
});


/***/ })

};
;